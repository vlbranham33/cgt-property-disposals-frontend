/*
 * Copyright 2019 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import cats.syntax.either._
import cats.syntax.eq._
import cats.instances.list._
import cats.instances.string._
import ltbs.uniform.{ErrorMsg, ErrorTree, Input, RichInput, Rule, UniformMessages}
import ltbs.uniform.TreeLike.ops._
import ltbs.uniform.common.web._
import ltbs.uniform.interpreters.playframework.Path
import play.twirl.api.Html
import uk.gov.hmrc.cgtpropertydisposalsfrontend.model.{Postcode, SelectedAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AddressLookupService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AddressLookupService.{Address, AddressLookupResult}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views

import scala.concurrent.{ExecutionContext, Future}

class AddressLookupWebAsk(
    addressLookupService: AddressLookupService,
    postcodeFormField: FormField[Postcode, Html]
)(implicit ec: ExecutionContext) extends GenericWebAsk[SelectedAddress, Html] {

  val postcodeId: List[String] = List("postcode")

  val addressLookupResultId: List[String] = List("address-lookup-result")

  def encodeAddressLookupResult(a: AddressLookupResult): Input = Input.one(
    a.postcode.value :: a.addresses.map(a => a.lines.mkString("||"))
  )

  def decodeAddressLookupResult(a: Input): Either[ErrorTree, AddressLookupResult] = {
      def fromString(s: String): Address = Address(s.split("\\|\\|").toList)

    a.valueAtRoot match {
      case None | Some(Nil)     => Left(ErrorMsg("required").toTree)
      case Some(postcode :: as) => Right(AddressLookupResult(as.map(fromString), Postcode(postcode)))
    }
  }

  def encodeSelectedAddress(a: SelectedAddress): Input =
    Input.one(a.postcode :: a.lines)

  def decodeSelectedAddress(a: Input): Either[ErrorTree, SelectedAddress] =
    a.valueAtRoot match {
      case None | Some(Nil)        => Left(ErrorMsg("required").toTree)
      case Some(postcode :: lines) => Right(SelectedAddress(lines, postcode))
    }

  override def page(
      targetId: List[String],
      currentId: List[String],
      default: Option[SelectedAddress],
      validation: List[List[Rule[SelectedAddress]]],
      config: JourneyConfig,
      submittedData: Option[Input],
      path: Path,
      db: DB,
      messages: UniformMessages[Html]
  ): Future[PageOut[SelectedAddress, Html]] = {
      def toString(selectedAddress: SelectedAddress): String =
        (selectedAddress.lines :+ selectedAddress.postcode).filter(_.nonEmpty).mkString(", ")

      def toStrings(addressLookupResult: AddressLookupResult): List[String] = {
          def toString(a: Address, postcode: Postcode): String =
            (a.lines :+ postcode.value).filter(_.nonEmpty).mkString(", ")

        addressLookupResult.addresses.map { toString(_, addressLookupResult.postcode) }
      }

      def dbObject[A](id: List[String], decode: Input => Either[ErrorTree, A]): Option[Either[ErrorTree, A]] =
        db.get(id).map(Input.fromUrlEncodedString(_).flatMap(decode))

    lazy val dbPostcode: Option[Either[ErrorTree, Postcode]] =
      dbObject(postcodeId, postcodeFormField.decode)

    lazy val dbAddressLookupResult: Option[Either[ErrorTree, AddressLookupResult]] =
      dbObject(addressLookupResultId, decodeAddressLookupResult)

    lazy val rawDbSelectedAddress = db.get(currentId)

    lazy val dbSelectedAddress: Option[Either[ErrorTree, SelectedAddress]] =
      dbObject(currentId, decodeSelectedAddress)

    val localData: Option[Input] = targetId.lastOption match {
      case Some(key) => submittedData.map(_.subTree(key))
      case None      => submittedData
    }

    if (targetId === currentId) {
      // check to see if anything was submitted...
      localData match {
        case None =>
          // check to see if there is postcode in DB - if not go back to postcode page
          (dbPostcode, dbAddressLookupResult) match {
            case (None, _) =>
              // no postcode - ask for postcode again
              Future.successful(PageOut(path, db, AskResult.GotoPath(postcodeId)))

            case (Some(Left(_)), _) =>
              // something's wrong with the postcode in db - clear it and ask for it again
              Future.successful(PageOut(path, db - postcodeId, AskResult.GotoPath(postcodeId)))

            case (Some(Right(postcode)), Some(Right(addressLookupResult))) if addressLookupResult.postcode === postcode =>
              // postcode is present and previous address lookup result is present which corresponds
              // to correct postcode

              val prepopulatedOption: Option[String] =
                rawDbSelectedAddress
                  .orElse(default.map(toString))

              val result = views.html.uniform.radios(
                currentId,
                toStrings(addressLookupResult),
                prepopulatedOption,
                ErrorTree.empty,
                messages
              )
              Future.successful(PageOut(path, db, AskResult.Payload(result, ErrorTree.empty)))

            case (Some(Right(postcode)), _) =>
              // postcode is present but the address lookup result cannot be used . One of the
              // following has happened:
              //   - address lookup hasn't happened yet
              //   - there's something wrong with the data in db
              //   - the postcode of the stored address lookup doesn't match the stored postcode
              addressLookupService.retrieveAddresses(postcode).map { addressLookupResult =>
                val result = views.html.uniform.radios(
                  currentId,
                  toStrings(addressLookupResult),
                  None,
                  ErrorTree.empty,
                  messages
                )

                val updatedDb = db + (addressLookupResultId -> encodeAddressLookupResult(addressLookupResult).toUrlEncodedString)
                PageOut(path, updatedDb, AskResult.Payload(result, ErrorTree.empty))
              }
          }

        case Some(rawPostData) =>
          // validate submitted data
          decodeSelectedAddress(rawPostData) match {
            case Left(e) =>
              // TODO: find options and pre-populated option in error case
              dbAddressLookupResult match {
                case None | Some(Left(_)) =>
                  // TODO: need to reconstruct options
                  sys.error("Shouldn't be here!")

                case Some(Right(a)) =>
                  val result = views.html.uniform.radios(
                    currentId,
                    toStrings(a),
                    None, // TODO: sort out
                    e,
                    messages
                  )
                  Future.successful(PageOut(path, db, AskResult.Payload(result, e)))
              }

            case Right(a) =>
              val updatedDb = db + (currentId -> rawPostData.toUrlEncodedString)
              Future.successful(PageOut(currentId :: path, updatedDb, AskResult.Success(a)))
          }
      }

    } else {
      Future.successful(PageOut(path, db, AskResult.GotoPath(targetId)))

      //      // work out if we're just replaying a journey or not
      //      dbSelectedAddress match {
      //        case Some(Right(data)) if targetId =!= Nil && !path.contains(targetId) =>
      //          // they're replaying the journey
      //           Future.successful(PageOut(currentId :: path, db, AskResult.Success(data)))
      //
      //        case _ =>
      //          Future.successful(PageOut(path, db, AskResult.GotoPath(currentId)))
      //      }
    }

  }

}

