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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding

import java.util.UUID

import cats.data.EitherT
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{AlreadySubscribedWithDifferentGGAccount, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionResponse.{AlreadySubscribed, SubscriptionSuccessful}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{SubscribedDetails, SubscriptionDetails, SubscriptionResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.{routes => onboardingRoutes}

import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class SubscriptionControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[SubscriptionService].toInstance(mockSubscriptionService)
    )

  lazy val controller = instanceOf[SubscriptionController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  val requestWithCSRFToken = FakeRequest().withCSRFToken

  val subscriptionDetails = sample[SubscriptionDetails]

  val ggCredId = sample[GGCredId]

  val sessionWithSubscriptionDetails =
    SessionData.empty.copy(journeyStatus = Some(SubscriptionReady(subscriptionDetails, ggCredId)))

  def mockSubscribe(expectedSubscriptionDetails: SubscriptionDetails)(response: Either[Error, SubscriptionResponse]) =
    (mockSubscriptionService
      .subscribe(_: SubscriptionDetails)(_: HeaderCarrier))
      .expects(expectedSubscriptionDetails, *)
      .returning(EitherT(Future.successful(response)))

  def redirectToStart(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: SubscriptionReady => true
        case _                    => false
      }
    )

  "The SubscriptionController" when {

    "handling requests to check subscription details" must {

      def performAction(): Future[Result] =
        controller.checkYourDetails()(requestWithCSRFToken)

      behave like redirectToStart(performAction)

      "show the check your details page" when {

        "there are subscription details in session for an individual" in {
          val individualSessionWithSubscriptionDetails =
            SessionData.empty.copy(
              journeyStatus = Some(
                SubscriptionReady(sample[SubscriptionDetails].copy(name = Right(sample[IndividualName])), ggCredId)
              )
            )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(individualSessionWithSubscriptionDetails))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("subscription.individual.title"))
        }

        "there are subscription details in session for an organisation" in {
          val organisationSessionWithSubscriptionDetails =
            SessionData.empty.copy(
              journeyStatus =
                Some(SubscriptionReady(sample[SubscriptionDetails].copy(name = Left(sample[TrustName])), ggCredId))
            )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(organisationSessionWithSubscriptionDetails))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("subscription.organisation.title"))
        }

      }

    }

    "handling submitted confirmation of subscription details" must {

      def performAction(): Future[Result] =
        controller.checkYourDetailsSubmit()(requestWithCSRFToken)

      val subscriptionSuccessfulResponse = SubscriptionSuccessful("number")
      val accountDetails = SubscribedDetails(
        subscriptionDetails.name,
        subscriptionDetails.emailAddress,
        subscriptionDetails.address,
        subscriptionDetails.contactName,
        CgtReference("number"),
        None,
        registeredWithId = true
      )

      val sessionWithSubscriptionComplete =
        SessionData.empty.copy(journeyStatus = Some(Subscribed(accountDetails, ggCredId)))

      behave like redirectToStart(performAction)

      "return an error" when {

        "there is an error during subscription" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionWithSubscriptionDetails))))
            mockSubscribe(subscriptionDetails)(Left(Error(new Exception(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionWithSubscriptionDetails))))
            mockSubscribe(subscriptionDetails)(Right(subscriptionSuccessfulResponse))
            mockStoreSession(sessionWithSubscriptionComplete)(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "redirect to the subscribed confirmation page" when {

        "subscription is successful and the session is updated successfully" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionWithSubscriptionDetails))))
            mockSubscribe(subscriptionDetails)(Right(subscriptionSuccessfulResponse))
            mockStoreSession(sessionWithSubscriptionComplete)(Future.successful(Right(())))
          }

          checkIsRedirect(performAction(), onboardingRoutes.SubscriptionController.subscribed())
        }

      }

      "redirect to the already subscribed with different gg account page" when {

        "the subscription response indicates that the user has already subscribed" in {
          val sessionWithAlreadySubscribed =
            SessionData.empty.copy(journeyStatus = Some(AlreadySubscribedWithDifferentGGAccount(ggCredId)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionWithSubscriptionDetails))))
            mockSubscribe(subscriptionDetails)(Right(AlreadySubscribed))
            mockStoreSession(sessionWithAlreadySubscribed)(Future.successful(Right(())))
          }

          checkIsRedirect(performAction(), onboardingRoutes.SubscriptionController.alreadySubscribedWithDifferentGGAccount())
        }

      }

    }

    "handling requests to display the subscribed page" must {

      def performAction(): Future[Result] =
        controller.subscribed()(FakeRequest())

      redirectToStartWhenInvalidJourney(
        performAction, {
          case _: Subscribed => true
          case _             => false
        }
      )

      "display the subscription confirmation page" when {

        "there is a subscription response and subscription details in session" in {
          val cgtReferenceNumber = UUID.randomUUID().toString
          val session = SessionData.empty.copy(
            journeyStatus = Some(
              Subscribed(
                SubscribedDetails(
                  subscriptionDetails.name,
                  subscriptionDetails.emailAddress,
                  subscriptionDetails.address,
                  subscriptionDetails.contactName,
                  CgtReference(cgtReferenceNumber),
                  None,
                  registeredWithId = true
                ),
                ggCredId
              )
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(session))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(cgtReferenceNumber)
          contentAsString(result) should include(message("subscribed.title"))

        }
      }

    }

    "handling requests to display the already subscribed with different gg account page" must {

      def performAction(): Future[Result] =
        controller.alreadySubscribedWithDifferentGGAccount()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case AlreadySubscribedWithDifferentGGAccount(_) => true
          case _                                          => false
        }
      )

      "display the page" when {

        "the session data indicates that the user has already subscribed with a different gg account" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(journeyStatus = Some(AlreadySubscribedWithDifferentGGAccount(ggCredId)))
                  )
                )
              )
            )
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("alreadySubscribedWithDifferentGGAccount.title"))

        }

      }

    }

    "handling requests to display the change gg account page" must {

      def performAction(): Future[Result] =
        controller.changeGGAccountForSubscription()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction, {
          case _: SubscriptionReady => true
          case _                    => false
        }
      )

      "display the page" when {

        "the session data indicates that the user has already subscribed with a different gg account" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    SessionData.empty.copy(journeyStatus = Some(sample[SubscriptionReady]))
                  )
                )
              )
            )
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("changeGGAccount.title"))

        }

      }

    }

  }

}