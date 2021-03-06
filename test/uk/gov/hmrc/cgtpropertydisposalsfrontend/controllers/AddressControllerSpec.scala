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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import cats.data.EitherT
import cats.instances.future._
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, AddressLookupResult, Country, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UKAddressLookupService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait AddressControllerSpec[J <: JourneyStatus]
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with PostcodeFormValidationTests {

  val validJourneyStatus: J

  def updateAddress(journey: J, address: Address): JourneyStatus

  val mockUpdateAddress: Option[(J, Address, Either[Error, Unit]) => Unit]

  val controller: AddressController[J]

  val mockService = mock[UKAddressLookupService]

  def mockAddressLookup(expectedPostcode: Postcode, filter: Option[String])(
    result: Either[Error, AddressLookupResult]
  ) =
    (mockService
      .lookupAddress(_: Postcode, _: Option[String])(_: HeaderCarrier))
      .expects(expectedPostcode, filter, *)
      .returning(EitherT.fromEither[Future](result))

  override def overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[UKAddressLookupService].toInstance(mockService),
      bind[SubscriptionService].toInstance(mockSubscriptionService)
    )

  val postcode = Postcode("AB1 2CD")

  def ukAddress(i: Int): UkAddress =
    UkAddress(s"$i the Street", Some("The Town"), None, None, postcode)

  val (addressHead, lastAddress, lastAddressIndex, addresses) = {
    val head = ukAddress(1)
    val last = ukAddress(5)
    (head, last, 4, head :: ((2 to 4).map(ukAddress).toList ::: List(last)))
  }
  val addressLookupResult = AddressLookupResult(postcode, None, addresses)

  lazy val sessionWithValidJourneyStatus =
    SessionData.empty.copy(journeyStatus = Some(validJourneyStatus))

  def displayIsUkBehaviour(
    performAction: () => Future[Result]
  )(implicit messagesApi: MessagesApi): Unit = {

    "show the page" when {
      "the address lookup results have been cleared from session" in {
        val session = sessionWithValidJourneyStatus.copy(
          addressLookupResult = Some(addressLookupResult)
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.copy(
              addressLookupResult = None
            )
          )(Right(()))
        }

        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) should include(messageFromMessageKey("subscription.isUk.title"))
      }

      "there are no address lookup results to clear from session" in {
        val session = sessionWithValidJourneyStatus

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) should include(messageFromMessageKey("subscription.isUk.title"))
      }
    }
    "display an error page" when {
      "there is an error when clearing address lookup results in session" in {
        val session = sessionWithValidJourneyStatus.copy(
          addressLookupResult = Some(addressLookupResult)
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.copy(
              addressLookupResult = None
            )
          )(Left(Error("")))
        }

        checkIsTechnicalErrorPage(performAction())
      }
    }

  }

  def submitIsUkBehaviour(
    performAction: Seq[(String, String)] => Future[Result],
    enterPostcode: Call,
    enterNonUkAddress: Call
  )(implicit messagesApi: MessagesApi): Unit = {

    "return a form error" when {
      "no selection has been made" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result = performAction(Seq.empty)
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("isUk.error.required"))
      }
    }

    "redirect to the enter postcode page" when {
      "Uk address has been selected" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result = performAction(Seq("isUk" -> "true"))
        checkIsRedirect(result, enterPostcode)
      }
    }

    "redirect to the enter non UK address page" when {
      "Non Uk address has been selected" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result = performAction(Seq("isUk" -> "false"))
        checkIsRedirect(result, enterNonUkAddress)
      }
    }
  }

  def displayEnterUkAddressPage(performAction: () => Future[Result])(implicit messagesApi: MessagesApi): Unit =
    "display the enter UK address page" when {
      val expectedTitleMessageKey =
        controller.addressJourneyType match {
          case AddressJourneyType.Returns => "address.uk.returns.title"
          case _                          => "address.uk.title"
        }

      "the endpoint is requested" in {
        val session = SessionData.empty.copy(
          journeyStatus       = Some(validJourneyStatus),
          addressLookupResult = Some(addressLookupResult)
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) should include(messageFromMessageKey(expectedTitleMessageKey))
      }
    }

  def submitEnterUkAddress(performAction: Seq[(String, String)] => Future[Result], continue: Call)(
    implicit messagesApi: MessagesApi
  ): Unit = {
    "return a form error" when {
      "address line 1 is empty" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result = performAction(Seq("postcode" -> "W1A2HV"))
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("address-line1.error.required"))
      }
      "address line 1 is too long" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result = performAction(
          Seq("address-line1" -> "1290b StreetWithAVeryLongNameForTestingTheMaxLength", "postcode" -> "W1A2HV")
        )
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("address-line1.error.tooLong"))
      }
      "address line 1 is invalid" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result = performAction(Seq("address-line1" -> "ContainsIllegal={%}=Characters", "postcode" -> "W1A2HV"))
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("address-line1.error.pattern"))
      }
      "address line 2 is too long" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result = performAction(
          Seq(
            "address-line1" -> "12 Valid Street",
            "address-line2" -> "StreetWithAVeryLongNameForTestingTheMaxLength",
            "postcode"      -> "W1A2HV"
          )
        )
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("address-line2.error.tooLong"))
      }
      "address postcode is empty" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result = performAction(Seq("address-line1" -> "Some street"))
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("postcode.error.required"))
      }

      "the address postcode contains invalid characters" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result = performAction(Seq("address-line1" -> "Some street", "postcode" -> "W1A,2HV"))
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("postcode.error.invalidCharacters"))
      }

      "the address postcode does not have a valid format" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result = performAction(Seq("address-line1" -> "Some street", "postcode" -> "ABC123"))
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("postcode.error.pattern"))
      }

    }
    "display an error page" when {

      mockUpdateAddress.foreach { mockAddressUpdate =>
        "the address cannot be updated" in {
          val newAddress = UkAddress("Test street", None, None, None, Postcode("W1A2HR"))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithValidJourneyStatus)
            mockAddressUpdate(validJourneyStatus, newAddress, Left(Error("")))
          }
          checkIsTechnicalErrorPage(performAction(Seq("address-line1" -> "Test street", "postcode" -> "W1A2HR")))
        }
      }

      "the address cannot be stored in the session" in {
        val newAddress = UkAddress("Test street", None, None, None, Postcode("W1A2HR"))
        val updatedSession = sessionWithValidJourneyStatus.copy(
          journeyStatus = Some(updateAddress(validJourneyStatus, newAddress))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
          mockUpdateAddress.foreach(_(validJourneyStatus, newAddress, Right(())))
          mockStoreSession(updatedSession)(Left(Error("")))
        }
        checkIsTechnicalErrorPage(performAction(Seq("address-line1" -> "Test street", "postcode" -> "W1A2HR")))
      }
    }

    s"redirect to ${continue.url}" when {
      "address has been stored in session" in {
        val newAddress =
          UkAddress("Flat 1", Some("The Street"), Some("The Town"), Some("Countyshire"), Postcode("W1A2HR"))
        val updatedSession = sessionWithValidJourneyStatus.copy(
          journeyStatus = Some(updateAddress(validJourneyStatus, newAddress))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
          mockUpdateAddress.foreach(_(validJourneyStatus, newAddress, Right(())))
          mockStoreSession(updatedSession)(Right(()))
        }
        val result = performAction(
          Seq(
            "address-line1"  -> "Flat 1",
            "address-line2"  -> "The Street",
            "address-town"   -> "The Town",
            "address-county" -> "Countyshire",
            "postcode"       -> "W1A2HR"
          )
        )
        checkIsRedirect(result, continue)
      }
    }
  }

  def displayEnterNonUkPage(performAction: () => Future[Result])(implicit messagesApi: MessagesApi): Unit =
    "display the enter non UK address page" when {
      "the endpoint is requested" in {
        val session = SessionData.empty.copy(
          journeyStatus       = Some(validJourneyStatus),
          addressLookupResult = None
        )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) should include(messageFromMessageKey("nonUkAddress.title"))
      }
    }

  def submitEnterNonUkAddress(performAction: Seq[(String, String)] => Future[Result], continue: Call)(
    implicit messagesApi: MessagesApi
  ): Unit = {
    "return a form error" when {
      "address line 1 is empty" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result = performAction(Seq("countryCode" -> "NZ"))
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("nonUkAddress-line1.error.required"))
      }
      "address line 1 is too long" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result = performAction(
          Seq("nonUkAddress-line1" -> "1290b StreetWithAVeryLongNameForTestingTheMaxLength", "countryCode" -> "NZ")
        )
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("nonUkAddress-line1.error.tooLong"))
      }
      "address line 1 is invalid" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result =
          performAction(Seq("nonUkAddress-line1" -> "12 ContainsIllegal={%}=Characters", "countryCode" -> "NZ"))
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("nonUkAddress-line1.error.pattern"))
      }
      "address line 2 is invalid" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result = performAction(
          Seq(
            "nonUkAddress-line1" -> "1290b Valid Street",
            "nonUkAddress-line2" -> "ContainsIllegal={%}=Characters",
            "countryCode"        -> "NZ"
          )
        )
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("nonUkAddress-line2.error.pattern"))
      }
      "countryCode is empty" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result = performAction(Seq("nonUkAddress-line1" -> "10 Some Street"))
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("countryCode.error.required"))
      }
      "countryCode is invalid" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
        val result = performAction(Seq("countryCode" -> "XX"))
        status(result)          shouldBe BAD_REQUEST
        contentAsString(result) should include(messageFromMessageKey("countryCode.error.notFound"))
      }
    }

    "display an error page" when {

      mockUpdateAddress.foreach { mockAddressUpdate =>
        "the address cannot be updated" in {
          val newAddress = NonUkAddress("House 1", None, None, None, None, Country("NZ", Some("New Zealand")))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithValidJourneyStatus)
            mockAddressUpdate(validJourneyStatus, newAddress, Left(Error("")))
          }
          checkIsTechnicalErrorPage(performAction(Seq("nonUkAddress-line1" -> "House 1", "countryCode" -> "NZ")))
        }
      }

      "the address cannot be stored in the session" in {
        val newAddress = NonUkAddress("House 1", None, None, None, None, Country("NZ", Some("New Zealand")))
        val updatedSession = sessionWithValidJourneyStatus.copy(
          journeyStatus = Some(updateAddress(validJourneyStatus, newAddress))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
          mockUpdateAddress.foreach(_(validJourneyStatus, newAddress, Right(())))
          mockStoreSession(updatedSession)(Left(Error("")))
        }
        checkIsTechnicalErrorPage(performAction(Seq("nonUkAddress-line1" -> "House 1", "countryCode" -> "NZ")))
      }
    }

    "redirect to the continue page" when {
      "address has been stored in session" in {
        val newAddress =
          NonUkAddress(
            "Flat 1",
            Some("The Street"),
            Some("The Town"),
            None,
            None,
            Country("NZ", Some("New Zealand"))
          )

        val updatedJourney = updateAddress(validJourneyStatus, newAddress)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
          mockUpdateAddress.foreach(_(validJourneyStatus, newAddress, Right(())))

          mockStoreSession(
            sessionWithValidJourneyStatus.copy(
              journeyStatus = Some(updatedJourney)
            )
          )(Right(()))
        }
        val result = performAction(
          Seq(
            "nonUkAddress-line1" -> "Flat 1",
            "nonUkAddress-line2" -> "The Street",
            "nonUkAddress-line3" -> "The Town",
            "countryCode"        -> "NZ"
          )
        )
        checkIsRedirect(result, continue)
      }
    }
  }

  def enterPostcodePage(performAction: () => Future[Result])(implicit messagesApi: MessagesApi): Unit =
    "display the enter postcode page" when {
      val expectedTitleMessageKey =
        controller.addressJourneyType match {
          case AddressJourneyType.Returns => "enterPostcode.returns.title"
          case _                          => "enterPostcode.title"
        }

      "there is no address lookup result in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }

        contentAsString(performAction()) should include(messageFromMessageKey(expectedTitleMessageKey))
      }

      "there is an address lookup result in session" in {
        val addressLookupResult = AddressLookupResult(postcode, None, List.empty)
        val session = sessionWithValidJourneyStatus.copy(
          addressLookupResult = Some(addressLookupResult)
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val content = contentAsString(performAction())
        content should include(messageFromMessageKey(expectedTitleMessageKey))
        content should include(s"""value="${postcode.value}"""")
      }

    }

  def submitEnterPostcode(performAction: Seq[(String, String)] => Future[Result], selectAddress: Call)(
    implicit messagesApi: MessagesApi
  ): Unit = {

    behave like commonPostcodeFormValidationTests(
      performAction,
      () =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }
    )

    "show form errors when no results are found for a postcode" in {
      val p                   = Postcode("NW19AX")
      val addressLookupResult = AddressLookupResult(p, None, List())
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(sessionWithValidJourneyStatus)
        mockAddressLookup(p, None)(Right(addressLookupResult))
        mockStoreSession(sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult)))(Right(()))
      }
      val result = performAction(Seq("postcode" -> p.value))
      status(result)          shouldBe BAD_REQUEST
      contentAsString(result) should include(messageFromMessageKey("postcode.error.noResults"))
    }

    "show form errors when no results are found for a filter" in {
      val p                   = Postcode("NW19AX")
      val filter              = "Some filter"
      val addressLookupResult = AddressLookupResult(p, Some(filter), List())
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(sessionWithValidJourneyStatus)
        mockAddressLookup(p, Some(filter))(Right(addressLookupResult))
        mockStoreSession(sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult)))(Right(()))
      }
      val result = performAction(Seq("postcode" -> p.value, "filter" -> filter))
      status(result)          shouldBe BAD_REQUEST
      contentAsString(result) should include(messageFromMessageKey("filter.error.noResults"))
    }

    "show form errors when no results are found for a postcode within a stored search" in {
      val p                   = Postcode("NW19AX")
      val addressLookupResult = AddressLookupResult(p, None, List())
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult)))
      }
      val result = performAction(Seq("postcode" -> p.value))
      status(result)          shouldBe BAD_REQUEST
      contentAsString(result) should include(messageFromMessageKey("postcode.error.noResults"))
    }

    "show form errors when no results are found for a filter within a stored search" in {
      val p                   = Postcode("NW19AX")
      val filter              = "Some filter"
      val addressLookupResult = AddressLookupResult(p, Some(filter), List())
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult)))
      }
      val result = performAction(Seq("postcode" -> p.value, "filter" -> filter))
      status(result)          shouldBe BAD_REQUEST
      contentAsString(result) should include(messageFromMessageKey("filter.error.noResults"))
    }

    "show an error page" when {

      "address lookup fails" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
          mockAddressLookup(postcode, None)(Left(Error("Uh oh!")))
        }

        checkIsTechnicalErrorPage(performAction(Seq("postcode" -> postcode.value)))
      }

      "the address lookup result cannot be stored in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
          mockAddressLookup(postcode, None)(Right(addressLookupResult))
          mockStoreSession(sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult)))(
            Left(Error("Uh oh!"))
          )
        }

        checkIsTechnicalErrorPage(performAction(Seq("postcode" -> postcode.value)))
      }

    }

    "redirect to select address without doing an address lookup" when {

      "there is an address lookup result already in session for the same postcode" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult)))
        }

        val result = performAction(Seq("postcode" -> postcode.value))
        checkIsRedirect(result, selectAddress)
      }

    }

    "redirect to select address when the address lookup result has been stored in mongo " +
      "and the postcode is valid" in {
      List(
        "AA9A 9AA",
        "A9A 9AA",
        "A9 9AA",
        "A99 9AA",
        "AA9 9AA",
        "AA99 9AA",
        "  aA99 9Aa ",
        "BFPO1",
        "BFPO12",
        "BFPO12 3",
        " BfpO123 ",
        "BFPO 123"
      ).foreach { postcode =>
        withClue(s"For postcode '$postcode': ") {
          val formattedPostcode = Postcode(postcode.trim)
          val addressLookupResult =
            AddressLookupResult(formattedPostcode, None, List(sample[UkAddress]))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithValidJourneyStatus)
            mockAddressLookup(formattedPostcode, None)(Right(addressLookupResult))
            mockStoreSession(sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult)))(
              Right(())
            )
          }

          val result = performAction(Seq("postcode" -> postcode))
          checkIsRedirect(result, selectAddress)
        }

      }

    }

    "trim leading and trailing spaces in postcodes" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(sessionWithValidJourneyStatus)
        mockAddressLookup(postcode, None)(Right(addressLookupResult))
        mockStoreSession(sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult)))(Right(()))
      }

      val result = performAction(Seq("postcode" -> s"  ${postcode.value}  "))
      checkIsRedirect(result, selectAddress)
    }

    "be ok with empty filter field and redirect to select address page" in {
      val filter = ""
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(sessionWithValidJourneyStatus)
        mockAddressLookup(postcode, None)(Right(addressLookupResult))
        mockStoreSession(sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult)))(Right(()))
      }
      val result = performAction(Seq("filter" -> filter, "postcode" -> postcode.value))
      checkIsRedirect(result, selectAddress)
    }

    "redirect to select address page when filter is submitted" in {
      val filter = "1"
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(sessionWithValidJourneyStatus)
        mockAddressLookup(postcode, Some(filter))(Right(addressLookupResult))
        mockStoreSession(sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult)))(Right(()))
      }
      val result = performAction(Seq("filter" -> filter, "postcode" -> postcode.value))
      checkIsRedirect(result, selectAddress)
    }

  }

  def displaySelectAddress(performAction: () => Future[Result], whenNoAddressLookupResult: Call)(
    implicit messagesApi: MessagesApi
  ): Unit = {
    s"redirect to ${whenNoAddressLookupResult.url}" when {

      "there is not address lookup result in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }

        val result = performAction()
        checkIsRedirect(result, whenNoAddressLookupResult)
      }

    }

    "display the select address page" when {
      val expectedMessageTitleKey = controller.addressJourneyType match {
        case AddressJourneyType.Returns => "address-select.returns.title"
        case _                          => "address-select.title"
      }

      "there is an address lookup result in session" in {
        val session = sessionWithValidJourneyStatus.copy(
          addressLookupResult = Some(addressLookupResult)
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) should include(messageFromMessageKey(expectedMessageTitleKey))
      }

    }

  }

  def submitSelectAddress(
    performAction: Seq[(String, String)] => Future[Result],
    whenNoAddressLookupResult: => Call,
    continue: => Call
  )(implicit messagesApi: MessagesApi): Unit = {
    val sessionWithValidJourneyStatusAndAddressLookupResult =
      sessionWithValidJourneyStatus.copy(
        addressLookupResult = Some(addressLookupResult)
      )

    "redirect to the check your details page" when {

      "there is no address lookup result in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }

        checkIsRedirect(performAction(Seq.empty), whenNoAddressLookupResult)
      }

    }

    "show a form error" when {

      "the submitted index is not valid" in {
        List(
          "-1"                                        -> "address-select.invalid",
          addressLookupResult.addresses.size.toString -> "address-select.invalid",
          "a"                                         -> "address-select.error.number"
        ).foreach {
          case (submitted, errorKey) =>
            withClue(s"For submitted data '$submitted': ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(sessionWithValidJourneyStatusAndAddressLookupResult)
              }

              val result = performAction(Seq("address-select" -> submitted))
              status(result)          shouldBe BAD_REQUEST
              contentAsString(result) should include(messageFromMessageKey(errorKey))
            }
        }
      }

    }

    "show an error page" when {

      mockUpdateAddress.foreach { mockAddressUpdate =>
        "the address cannot be updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithValidJourneyStatusAndAddressLookupResult)
            mockAddressUpdate(validJourneyStatus, lastAddress, Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(Seq("address-select" -> s"$lastAddressIndex")))
        }
      }

      "the selected address cannot be stored in session" in {
        val updatedSession = sessionWithValidJourneyStatusAndAddressLookupResult.copy(
          journeyStatus = Some(updateAddress(validJourneyStatus, lastAddress))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatusAndAddressLookupResult)
          mockUpdateAddress.foreach(_(validJourneyStatus, lastAddress, Right(())))
          mockStoreSession(updatedSession)(Left(Error("")))
        }

        checkIsTechnicalErrorPage(performAction(Seq("address-select" -> s"$lastAddressIndex")))
      }

    }

    "redirect to the continue endpoint" when {

      "the selected address is stored in session" in {
        val updatedSession = sessionWithValidJourneyStatusAndAddressLookupResult.copy(
          journeyStatus = Some(updateAddress(validJourneyStatus, lastAddress))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatusAndAddressLookupResult)
          mockUpdateAddress.foreach(_(validJourneyStatus, lastAddress, Right(())))
          mockStoreSession(updatedSession)(Right(()))
        }

        val result = performAction(Seq("address-select" -> s"$lastAddressIndex"))
        checkIsRedirect(result, continue)
      }

    }

  }

}
