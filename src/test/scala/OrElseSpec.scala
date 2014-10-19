package com.github.xuwei_k.scalajb

import argonaut.{CodecJson, DecodeJson, EncodeJson}
import org.scalacheck.{Arbitrary, Prop, Properties}
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.{Equal, Functor, \/}
import scalaz.std.anyVal._
import scalaz.std.string._

object OrElseSpec extends Properties("OrElse"){

  private def encodedecode[A: EncodeJson: DecodeJson: Arbitrary: Equal] =
    Prop.forAll(CodecJson.derived[A].codecLaw.encodedecode(_))

  implicit def orElseArb[A: Arbitrary, B: Arbitrary]: Arbitrary[A OrElse B] =
    Functor[Arbitrary].map(implicitly[Arbitrary[A \/ B]])(OrElse.apply)

  property("OrElse encode/decode") = encodedecode[Int OrElse String]
}
