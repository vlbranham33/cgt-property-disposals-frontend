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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions

import cats.syntax.either._
import com.google.inject.{Inject, Singleton}
import play.api.i18n.MessagesApi
import play.api.mvc._
import play.api.mvc.Results.SeeOther
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{SessionData, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

final case class RequestWithSubscriptionDetails[A](
    subscriptionDetails: SubscriptionDetails,
    sessionData: SessionData,
    authenticatedRequest: AuthenticatedRequest[A]
) extends WrappedRequest[A](authenticatedRequest) with PreferredMessagesProvider {
  override def messagesApi: MessagesApi = authenticatedRequest.request.messagesApi
}

@Singleton
class SubscriptionDetailsAction @Inject() (
    sessionStore: SessionStore,
    playBodyParsers: PlayBodyParsers,
    errorHandler: ErrorHandler
)(implicit ec: ExecutionContext) extends ActionRefiner[AuthenticatedRequest, RequestWithSubscriptionDetails] with Logging {

  override protected def executionContext: ExecutionContext = ec

  override protected def refine[A](request: AuthenticatedRequest[A]): Future[Either[Result, RequestWithSubscriptionDetails[A]]] = {
    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

    sessionStore.get().map(
      _.leftMap { e =>
        logger.warn("Could not get session data", e)
        errorHandler.errorResult()(request)
      }
        .flatMap { d =>
          (d, d.flatMap(_.subscriptionDetails)) match {
            case (Some(sessionData), Some(subscriptionDetails)) =>
              Right(RequestWithSubscriptionDetails(subscriptionDetails, sessionData, request))

            case _ =>
              Left(SeeOther(routes.StartController.start().url))

          }
        }
    )
  }

}
