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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.views.account

import scala.reflect.ClassTag

sealed trait AccountMenuItem
object AccountMenuItem {
  final case class Home() extends AccountMenuItem
  final case class ManageYourDetails() extends AccountMenuItem

  implicit class AccountMenuItemOps(val a: AccountMenuItem) extends AnyVal {
    def is[A <: AccountMenuItem: ClassTag]: Boolean = a match {
      case _: A => true
      case _ => false
    }
  }

}
