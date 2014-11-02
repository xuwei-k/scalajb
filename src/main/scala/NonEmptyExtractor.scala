package com.github.xuwei_k.scalajb

import unfiltered.request.Params

sealed abstract class NonEmptyExtractor[A] {
  def unapply(map: Params.Map): NonEmpty[A]

  def map[B](f: A => B): NonEmptyExtractor[B] =
    NonEmptyExtractor(m => f(unapply(m).get))
}

object NonEmptyExtractor {
  def apply[A](f: Params.Map => A): NonEmptyExtractor[A] =
    new NonEmptyExtractor[A] {
      def unapply(map: Params.Map) =
        NonEmpty(f(map))
    }
}
