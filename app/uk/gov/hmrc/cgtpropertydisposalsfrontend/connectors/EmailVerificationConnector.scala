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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors

import java.util.UUID

import configs.syntax._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import configs.Configs
import play.api.Configuration
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.EmailVerificationConnectorImpl.EmailVerificationRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes
import uk.gov.hmrc.cgtpropertydisposalsfrontend.http.HttpClient._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Email
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration

@ImplementedBy(classOf[EmailVerificationConnectorImpl])
trait EmailVerificationConnector {

  def verifyEmail(email: Email, id: UUID, name: String)(implicit hc: HeaderCarrier): Future[HttpResponse]

}

@Singleton
class EmailVerificationConnectorImpl @Inject() (
    http: HttpClient,
    config: Configuration
)(implicit ec: ExecutionContext) extends EmailVerificationConnector {

  def getEmailVerificationConfig[A: Configs](key: String): A = config.underlying.get[A](s"microservice.services.email-verification.$key").value

  val url: String = {
    val protocol = getEmailVerificationConfig[String]("protocol")
    val host = getEmailVerificationConfig[String]("host")
    val port = getEmailVerificationConfig[String]("port")
    s"$protocol://$host:$port/email-verification/verification-requests"
  }

  val templateId: String = getEmailVerificationConfig[String]("template-id")

  val linkExpiryTime: String = {
    val minutes = getEmailVerificationConfig[FiniteDuration]("link-expiry-time").toMinutes
    java.time.Duration.ofMinutes(minutes).toString
  }

  val selfBaseUrl: String = config.underlying.get[String]("self.url").value

  def continueUrl(id: UUID): String = {
    s"$selfBaseUrl${routes.EmailController.verifyEmail(id).url}"
  }

  def verifyEmail(email: Email, id: UUID, name: String)(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    val body =
      EmailVerificationRequest(
        email.value,
        templateId,
        linkExpiryTime,
        s"${continueUrl(id)}",
        Map("name" -> name)
      )
    http.post(url, Json.toJson(body))
  }

}

object EmailVerificationConnectorImpl {

  final case class EmailVerificationRequest(
      email: String,
      templateId: String,
      linkExpiryDuration: String,
      continueUrl: String,
      templateParameters: Map[String, String]
  )

  implicit val formats: Format[EmailVerificationRequest] = Json.format[EmailVerificationRequest]

}