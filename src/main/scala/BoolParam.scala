package com.github.xuwei_k.scalajb

import unfiltered.request.Params

final case class BoolParam(key: String, default: Boolean) {

  val Extractor: NonEmptyExtractor[Boolean] =
    Web.booleanParam(key, default)

  def unapply(map: Params.Map): NonEmpty[Boolean] =
    Extractor.unapply(map)

}
