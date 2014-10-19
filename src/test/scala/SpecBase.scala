package com.github.xuwei_k.scalajb

import scalaj.http.{Http => ScalajHttp, _}
import org.specs2.mutable.Specification

abstract class SpecBase extends Specification with unfiltered.specs2.jetty.Served {

  override def setup = {
    _.plan(new Web)
  }

  protected[this] final val OPTIONS = List(HttpOptions.connTimeout(30000), HttpOptions.readTimeout(30000))

  protected[this] def Scalaj(path: String) =
    ScalajHttp("http://localhost:" + port + "/" + path).options(OPTIONS)
}
