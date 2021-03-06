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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns

import cats.syntax.order._
import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers

sealed trait CalculatedTaxDue extends Product with Serializable {
  val disposalAmountLessCosts: AmountInPence
  val acquisitionAmountPlusCosts: AmountInPence
  val initialGainOrLoss: AmountInPence
  val totalReliefs: AmountInPence
  val gainOrLossAfterReliefs: AmountInPence
  val totalLosses: AmountInPence
  val gainOrLossAfterLosses: AmountInPence
  val taxableGainOrNetLoss: AmountInPence
  val yearToDateLiability: AmountInPence
  val amountOfTaxDue: AmountInPence
}

object CalculatedTaxDue {

  final case class NonGainCalculatedTaxDue(
    disposalAmountLessCosts: AmountInPence,
    acquisitionAmountPlusCosts: AmountInPence,
    initialGainOrLoss: AmountInPence,
    totalReliefs: AmountInPence,
    gainOrLossAfterReliefs: AmountInPence,
    totalLosses: AmountInPence,
    gainOrLossAfterLosses: AmountInPence,
    taxableGainOrNetLoss: AmountInPence,
    yearToDateLiability: AmountInPence,
    amountOfTaxDue: AmountInPence
  ) extends CalculatedTaxDue

  final case class GainCalculatedTaxDue(
    disposalAmountLessCosts: AmountInPence,
    acquisitionAmountPlusCosts: AmountInPence,
    initialGainOrLoss: AmountInPence,
    totalReliefs: AmountInPence,
    gainOrLossAfterReliefs: AmountInPence,
    totalLosses: AmountInPence,
    gainOrLossAfterLosses: AmountInPence,
    taxableGainOrNetLoss: AmountInPence,
    taxableIncome: AmountInPence,
    taxDueAtLowerRate: TaxableAmountOfMoney,
    taxDueAtHigherRate: TaxableAmountOfMoney,
    yearToDateLiability: AmountInPence,
    amountOfTaxDue: AmountInPence
  ) extends CalculatedTaxDue
  @SuppressWarnings(Array("org,wartremover.warts.PublicInference"))
  implicit val format: OFormat[CalculatedTaxDue] = derived.oformat()

}
