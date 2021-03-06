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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.config

import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{routes, _}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

@Singleton
class ViewConfig @Inject() (servicesConfig: ServicesConfig) {

  private def getString(key: String): String = servicesConfig.getString(key)

  private val companyAuthUrl: String       = getString("company-auth-frontend.url")
  private val signOutUri: String           = getString("sign-out.uri")
  private val contactFormServiceIdentifier = "CGTPD"

  val assetsPrefix: String   = getString("assets.url") + getString("assets.version")
  val analyticsToken: String = getString("google-analytics.token")
  val analyticsHost: String  = getString("google-analytics.host")
  val reportAProblemPartialUrl: String =
    s"/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"
  val reportAProblemNonJSUrl: String =
    s"/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"
  val betaFeedbackUrlNoAuth: String =
    s"/contact/beta-feedback-unauthenticated?service=$contactFormServiceIdentifier"
  val govUkUrl: String                         = getString("external-url.gov-uk")
  val signOutUrl: String                       = s"$companyAuthUrl$signOutUri"
  val trustRegistrationUrl: String             = getString("external-url.trust-registration")
  val callChargesUrl: String                   = getString("external-url.gov-call-charges")
  val additionalNeedsUrl: String               = getString("external-url.additional-needs")
  val taxSellHomeUrl: String                   = getString("external-url.tax-sell-home")
  val taxSellPropertyUrl: String               = getString("external-url.tax-sell-property")
  val cgtUrl: String                           = getString("external-url.capital-gains-tax")
  val cgtLegacyUrl: String                     = getString("external-url.capital-gains-tax-legacy")
  val legacyCgtNonResidentUrl: String          = getString("external-url.legacy-cgt-non-resident")
  val agentsSignInUrl: String                  = getString("external-url.sign-in-to-agents")
  val createAgentsAccountUrl: String           = getString("external-url.create-agents-account")
  val lostUtrUrl: String                       = getString("external-url.lost-utr")
  val trusteeResponsibilitiesUrl: String       = getString("external-url.trustee-responsibilities")
  val fileCorporationTaxUrl: String            = getString("external-url.file-corporation-tax")
  val trustHelpUrl: String                     = getString("external-url.trust-help")
  val tellHmrcChangeDetails: String            = getString("external-url.tell-hmrc-change-details")
  val workOurYouResidenceStatusUrl: String     = getString("external-url.work-out-your-residence-status")
  val residentialPropertyDefinitionUrl: String = getString("external-url.residential-property-definition")
  val tranferringOwnershipHelp: String         = getString("external-url.transferring-ownership-help")
  val cgtLossesUrl: String                     = getString("external-url.cgt-losses")
  val annualExemptAmountUrl: String            = getString("external-url.annual-exempt-amount")
  val personalAllowanceUrl: String             = getString("external-url.personal-allowance")
  val selfAssessmentUrl: String                = getString("external-url.sign-in-to-self-assessment")
  val reportingCgtBefore6April2020: String     = getString("external-url.reporting-cgt-before-6-april-2020")

  val onboardingExitSurveyUrl: String = "/feedback/CGTPD-REG"
  val returnsExitSurveyUrl: String    = "/feedback/CGTPD-RET"
  val ggCreateAccountUrl: String =
    "/bas-gateway?" +
      "accountType=individual&" +
      "continueUrl=%2Fcapital-gains-tax-uk-property%2Fstart&" +
      "origin=capital-gains-tax-uk-property-frontend"
  val ggTimeoutSeconds: Long   = servicesConfig.getDuration("gg.timeout").toSeconds
  val ggCountdownSeconds: Long = servicesConfig.getDuration("gg.countdown").toSeconds
  val ggKeepAliveUrl: String   = "/capital-gains-tax-uk-property" + routes.StartController.keepAlive().url
  val ggTimedOutUrl: String = signOutUrl + "?continue=/capital-gains-tax-uk-property" + routes.StartController
    .timedOut()
    .url
  val ggSignOut: String = signOutUrl + "?continue=/capital-gains-tax-uk-property" + routes.StartController.start().url
  val accountSignOutUrl
    : String = signOutUri + "?continue=/capital-gains-tax-uk-property" + accounts.routes.AccountController
    .signedOut()
    .url
  val gaUserTypeDimension: String = getString("google-analytics.user-type-dimension")

}
