package com.github.xuwei_k.scalajb

import unfiltered.request.Params

sealed abstract class NonEmptyExtractor[A] {
  def unapply(map: Params.Map): NonEmpty[A]
}

object NonEmptyExtractor {
  def apply[A](f: Params.Map => A): NonEmptyExtractor[A] =
    new NonEmptyExtractor[A] {
      def unapply(map: Params.Map) =
        NonEmpty(f(map))
    }
}
