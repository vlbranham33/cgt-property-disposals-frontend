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

import com.google.inject.{ImplementedBy, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.model.Postcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AddressLookupService.{Address, AddressLookupResult}

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[AddressLookupServiceImpl])
trait AddressLookupService {

  def retrieveAddresses(postcode: Postcode)(implicit ec: ExecutionContext): Future[AddressLookupResult]

}

object AddressLookupService {

  final case class Address(lines: List[String])

  // TODO: add timestamp to allow for caching and refreshing
  final case class AddressLookupResult(addresses: List[Address], postcode: Postcode)

}

@Singleton
class AddressLookupServiceImpl extends AddressLookupService {

  override def retrieveAddresses(postcode: Postcode)(implicit ec: ExecutionContext): Future[AddressLookupResult] = {
    println("Service returning addresses\n\n")
    Future.successful(
      AddressLookupResult(
        List(
          Address(List("1 the street", "The Town", "The County")),
          Address(List("2 the street", "The Town", "The County")),
          Address(List("3 the street", "The Town", "The County"))
        ),
        postcode
      )
    )
  }

}
