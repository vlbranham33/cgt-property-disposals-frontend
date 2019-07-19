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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.model

import cats.{Eq, Monad}
import cats.instances.string._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.eq._
import ltbs.uniform.{Language, NilTypes}

import scala.language.higherKinds
import ltbs.uniform.::

final case class Postcode(value: String)

object Postcode {

  implicit val eq: Eq[Postcode] = Eq.instance(_.value === _.value)
}

final case class PropertyData(postcode: Postcode)

final case class SelectedAddress(lines: List[String], postcode: String)

object PropertyData {

  type TellTypes = NilTypes

  type AskTypes = SelectedAddress :: Postcode :: NilTypes

  def program[F[_]: Monad](
      interpreter: Language[F, TellTypes, AskTypes]
  ): F[SelectedAddress] = {
    import interpreter._
    for {
      _ <- ask[Postcode]("postcode")
      address <- ask[SelectedAddress]("address")
    } yield address
  }

}
