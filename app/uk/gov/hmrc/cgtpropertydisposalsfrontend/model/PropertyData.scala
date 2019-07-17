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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.model

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import ltbs.uniform.{Language, NilTypes}
import scala.language.higherKinds
import ltbs.uniform.::

final case class Postcode(value: String)

final case class Address(lines: List[String], town: String, county: String)

final case class PropertyData(postcode: Postcode)

abstract class AddressLookup[F[_]] {

  def retrieveAddresses(postcode: Postcode): F[List[Address]]

}

object PropertyData {

  type TellTypes = List[Address] :: NilTypes

  type AskTypes = Address :: Postcode :: NilTypes

  def program[F[_]: Monad](
      interpreter: Language[F, TellTypes, AskTypes],
      addressLookup: AddressLookup[F]
  ): F[List[Address]] = {
    import interpreter._
    for {
      postcode <- ask[Postcode]("postcode")
      addresses <- addressLookup.retrieveAddresses(postcode)
      address <- ask[Address]()
    } yield address
  }

}
