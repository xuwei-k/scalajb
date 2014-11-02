package com.github.xuwei_k.scalajb

import unfiltered.request.Params

final case class BoolParam(key: String, default: Boolean) {

  val uf: Params.Extract[Nothing, Boolean] =
    Web.booleanParam(key, default)

  def unapply(map: Params.Map): Option[Boolean] =
    uf.unapply(map)
}
