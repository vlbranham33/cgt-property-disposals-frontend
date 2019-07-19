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

import cats.data.NonEmptyList
import cats.implicits._
import cats.instances.either._
import com.google.inject.Inject
import play.api.i18n.I18nSupport
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.AppConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import ltbs.uniform._
import interpreters.playframework._
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import play.twirl.api.{Html, HtmlFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.model.{Postcode, PropertyData, SelectedAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AddressLookupService

import scala.concurrent.{ExecutionContext, Future}

class PropertyDetailsController @Inject() (
    addressLookupService: AddressLookupService,
    val messagesApi: MessagesApi,
    mainTemplate: uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.main_template
)(mcc: MessagesControllerComponents)(implicit appConfig: AppConfig, ec: ExecutionContext)
  extends PlayInterpreter[Html] with I18nSupport {

  override def messages(request: Request[AnyContent], customContent: Map[String, (String, List[Any])]): UniformMessages[Html] =
    this.convertMessages(messagesApi.preferred(request)) |+|
      UniformMessages.bestGuess.map(HtmlFormat.escape)

  override def pageChrome(
      key: List[String],
      errors: ErrorTree,
      tell: Html,
      ask: Html,
      breadcrumbs: _root_.ltbs.uniform.interpreters.playframework.Path,
      request: Request[AnyContent],
      messages: UniformMessages[Html]
  ): Html = {
    import cats.syntax.semigroup._
    val content = views.html.form_wrapper(
      key.lastOption.getOrElse(sys.error("Oh no!")), // is this ok?
      tell |+| ask,
      breadcrumbs
    )(messages, request)

    mainTemplate(
      title = s"${messages(key.mkString("-") + ".heading")} - ${messages("common.title")}"
    )(
      content
    )(request, messages, appConfig)
  }

  override def selectionOfFields(inner: List[(String, (List[String], _root_.ltbs.uniform.common.web.Path, Option[Input], ErrorTree, UniformMessages[Html]) => Html)])(key: List[String], path: _root_.ltbs.uniform.common.web.Path, values: Option[Input], errors: ErrorTree, messages: UniformMessages[Html]): Html = {
    Html("hello")
  }

  implicit val persistence: DebugPersistence = DebugPersistence(UnsafePersistence())

  implicit val twirlPostcodeField: FormField[Postcode, Html] = new FormField[Postcode, Html] {
    def decode(out: Input): Either[ErrorTree, Postcode] = {
      val root: Option[String] = out.valueAtRoot.flatMap(_.find(_.trim.nonEmpty))

      root match {
        case None       ⇒ ErrorMsg("required").toTree.asLeft[Postcode]
        case Some(data) ⇒ Postcode(data).asRight[ErrorTree]
      }
    }

    def encode(in: Postcode): Input = Input.one(List(in.value))

    def render(
        key: List[String],
        path: Path,
        data: Option[Input],
        errors: ErrorTree,
        messages: UniformMessages[Html]
    ): Html = {
      val existingValue: Option[String] = data.flatMap(_.valueAtRoot.flatMap { _.headOption })
      views.html.uniform.string(key, existingValue, errors, messages)
    }
  }

  implicit val addressLookupWebAsk: AddressLookupWebAsk = new AddressLookupWebAsk(addressLookupService, twirlPostcodeField)

  //  implicit val twirlAddressListHtml: GenericWebTell[List[Address], Html] = new GenericWebTell[List[Address], Html] {
  //    override def render(in: List[Address]): Html = Html(s"""${in.mkString("\n")}""")
  //  }

  def propertyDetails(id: String): Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>

    val playJourney: WebMonad[SelectedAddress] = PropertyData.program(
      new FuturePlayInterpreter[PropertyData.TellTypes, PropertyData.AskTypes]
    )

    run[SelectedAddress](playJourney, id) { out =>
      Future.successful(Ok(out.toString))
    }
  }

}
