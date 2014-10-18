package com.github.xuwei_k.scalajb

import scalaj.http.{Http => ScalajHttp, _}
import org.specs2.mutable.Specification

final class Spec extends Specification with unfiltered.specs2.jetty.Served {

  override def setup = { _ .filter(new Web) }

  val OPTIONS = List(HttpOptions.connTimeout(30000), HttpOptions.readTimeout(30000))

  def Scalaj(path:String) =
    ScalajHttp("http://localhost:" + port + "/" + path ).options(OPTIONS)

  private[this] val urls = Seq(
    "https://api.github.com/repos/xuwei-k/scalajb",
    "https://api.github.com/users/xuwei-k/repos"
  )

  "Web" should {
    def test(param: (String, String))(funcs: (String => Boolean) *) = {

      forall(List("", "api")){ path =>
        forall(urls){url =>
          val req = Scalaj(path).params("url"->url,param)
          req.responseCode must_== 200
          val str = req.asString
          println(str)
          forall(funcs){ f =>
            f(str) must beTrue
          }
        }
      }
    }

    "scala" in {
      test(("lang","scala"))(_.contains("case class"),_.contains("`private`"),! _.contains(" private "))
    }

    "java" in {
      test(("lang","java"))(_.contains("public class"),_.contains("_private"),! _.contains(" private "))
    }
  }
}

