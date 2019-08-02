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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services

import cats.instances.int._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BusinessPartnerRecord, Error, NINO}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[BusinessPartnerRecordServiceImpl])
trait BusinessPartnerRecordService {

  def getBusinessPartnerRecord(nino: NINO)(implicit hc: HeaderCarrier): Future[Either[Error, BusinessPartnerRecord]]

}

@Singleton
class BusinessPartnerRecordServiceImpl @Inject() (connector: CGTPropertyDisposalsConnector)(implicit ec: ExecutionContext) extends BusinessPartnerRecordService {

  override def getBusinessPartnerRecord(nino: NINO)(implicit hc: HeaderCarrier): Future[Either[Error, BusinessPartnerRecord]] =
    connector.getBusinessPartnerRecord(nino)
      .map { response =>
        if (response.status === 200) {
          response.parseJSON[BusinessPartnerRecord]().leftMap(Error.apply)
        } else {
          Left(Error(s"Call to get BPR came back with status ${response.status}"))
        }
      }
      .recover {
        case e => Left(Error(e))
      }

}
