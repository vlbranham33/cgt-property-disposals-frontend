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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding

import cats.data.NonEmptyList
import cats.syntax.either._
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, AddressSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, ContactNameSource, IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.{Email, EmailSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherUtils.eitherFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.SapNumber

final case class SubscriptionDetails(
  name: Either[TrustName, IndividualName],
  emailAddress: Email,
  address: Address,
  contactName: ContactName,
  sapNumber: SapNumber,
  emailSource: EmailSource,
  addressSource: AddressSource,
  contactNameSource: ContactNameSource
)

object SubscriptionDetails {

  implicit val format: Format[SubscriptionDetails] = Json.format

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def apply(
    bpr: BusinessPartnerRecord,
    ggEmail: Option[Email],
    enteredEmail: Option[Email]
  ): Either[NonEmptyList[MissingData], SubscriptionDetails] =
    Either
      .fromOption(
        enteredEmail
          .map(_ -> EmailSource.ManuallyEntered)
          .orElse(bpr.emailAddress.map(_ -> EmailSource.BusinessPartnerRecord))
          .orElse(ggEmail.map(_ -> EmailSource.GovernmentGateway)),
        NonEmptyList.one(MissingData.Email)
      )
      .map { emailWithSource =>
        SubscriptionDetails(
          bpr.name,
          emailWithSource._1,
          bpr.address,
          ContactName(bpr.name.fold(_.value, n => n.makeSingleName())),
          bpr.sapNumber,
          emailWithSource._2,
          AddressSource.BusinessPartnerRecord,
          ContactNameSource.DerivedFromBusinessPartnerRecord
        )
      }

  sealed trait MissingData extends Product with Serializable

  object MissingData {

    case object Email extends MissingData

  }

}
