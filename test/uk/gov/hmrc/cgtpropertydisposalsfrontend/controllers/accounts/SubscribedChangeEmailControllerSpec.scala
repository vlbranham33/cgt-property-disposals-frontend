/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.EmailControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedUpdateDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
class SubscribedChangeEmailControllerSpec
    extends EmailControllerSpec[Subscribed, Subscribed]
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  override val isAmendJourney: Boolean = true

  override val validJourneyStatus: Subscribed = sample[Subscribed]

  override val validVerificationCompleteJourneyStatus: Subscribed = validJourneyStatus

  override lazy val controller: SubscribedChangeEmailController = instanceOf[SubscribedChangeEmailController]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[EmailVerificationService].toInstance(mockService),
      bind[UUIDGenerator].toInstance(mockUuidGenerator),
      bind[SubscriptionService].toInstance(mockSubscriptionService)
    )

  def mockSubscriptionUpdate(subscribedUpdateDetails: SubscribedUpdateDetails)(result: Either[Error, Unit]) =
    (mockSubscriptionService
      .updateSubscribedDetails(_: SubscribedUpdateDetails)(_: HeaderCarrier))
      .expects(subscribedUpdateDetails, *)
      .returning(EitherT.fromEither[Future](result))

  override def updateEmail(journey: Subscribed, email: Email): Subscribed =
    journey.copy(subscribedDetails = journey.subscribedDetails.copy(emailAddress = email))

  override val mockUpdateEmail: Option[(Subscribed, Subscribed, Either[Error, Unit]) => Unit] = Some({
    case (oldDetails: Subscribed, newDetails: Subscribed, r: Either[Error, Unit]) =>
      mockSubscriptionUpdate(SubscribedUpdateDetails(newDetails.subscribedDetails, oldDetails.subscribedDetails))(r)
  })

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: Subscribed => true
        case _             => false
      }
    )

  "SubscribedChangeEmailController" when {

    "handling requests to display the enter email page" must {

      def performAction(): Future[Result] =
        controller.enterEmail()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)
      behave like enterEmailPage(performAction)

    }

    "handling submitted email addresses" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.enterEmailSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction())

      behave like enterEmailSubmit(
        performAction,
        validJourneyStatus.subscribedDetails.contactName,
        controllers.accounts.routes.SubscribedChangeEmailController.verifyEmail,
        controllers.accounts.routes.SubscribedChangeEmailController.checkYourInbox()
      )
    }

    "handling requests to display the check your inbox page" must {

      def performAction(): Future[Result] =
        controller.checkYourInbox()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like checkYourInboxPage(
        performAction,
        controllers.accounts.routes.SubscribedChangeEmailController.enterEmail(),
        controllers.accounts.routes.SubscribedChangeEmailController.enterEmail().url
      )
    }

    "handling requests to verify an email" must {

      def performAction(id: UUID): Future[Result] =
        controller.verifyEmail(id)(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction(UUID.randomUUID()))

      behave like verifyEmail(
        performAction,
        controllers.accounts.routes.SubscribedChangeEmailController.enterEmail(),
        controllers.accounts.routes.SubscribedChangeEmailController.emailVerified()
      )

    }

    "handling requests to display the email verified page" must {

      def performAction(): Future[Result] =
        controller.emailVerified()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like emailVerifiedPage(
        performAction,
        controllers.accounts.routes.AccountController.contactEmailUpdated(),
        routes.SubscribedChangeEmailController.enterEmail()
      )
    }
  }
}
