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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.name

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.name.RegistrationEnterNameController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.IndividualSupplyingInformation
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{JourneyStatus, Name}

class RegistrationEnterNameControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with NameControllerSpec[IndividualSupplyingInformation]
    with ScalaCheckDrivenPropertyChecks {

  override lazy val controller: RegistrationEnterNameController = instanceOf[RegistrationEnterNameController]

  override lazy val validJourney: IndividualSupplyingInformation = IndividualSupplyingInformation(None, None, None)

  def isValidJourney(journey: JourneyStatus): Boolean = journey match {
    case _: IndividualSupplyingInformation => true
    case _ => false
  }

  override def updateName(name: Name, journey: IndividualSupplyingInformation): JourneyStatus =
    journey.copy(name = Some(name))

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  "RegistrationEnterNameController" when {

    "handling requests to display the enter name page" must {

      behave like enterNamePage(
        () => controller.enterIndividualName()(FakeRequest())
      )

    }

    "handling submitted names" must {

      behave like enterNameSubmit(
        data => controller.enterIndividualNameSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*).withCSRFToken),
        controllers.address.routes.RegistrationEnterAddressController.isUk()
      )
    }

  }


}