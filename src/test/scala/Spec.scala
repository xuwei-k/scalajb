package com.github.xuwei_k.scalajb

import scalaj.http.{Http => ScalajHttp, _}
import org.specs2.mutable.Specification

sealed abstract class SpecBase extends Specification with unfiltered.specs2.jetty.Served {

  override def setup = {
    _.filter(new Web)
  }

  private val OPTIONS = List(HttpOptions.connTimeout(30000), HttpOptions.readTimeout(30000))

  protected[this] def Scalaj(path: String) =
    ScalajHttp("http://localhost:" + port + "/" + path).options(OPTIONS)
}

object JSONSpec extends SpecBase {

  "JSON" should {
    val urls: Seq[String] = Seq(
      "https://api.github.com/repos/xuwei-k/scalajb",
      "https://api.github.com/users/xuwei-k/repos"
    )

    def test(param: (String, String))(funcs: (String => Boolean)*) = {
      forall(List("", "api")) { path =>
        forall(urls) { url =>
          val req = Scalaj(path).params("url" -> url, param)
          req.responseCode must_== 200
          val str = req.asString
          println(str)
          forall(funcs) { f =>
            f(str) must beTrue
          }
        }
      }
    }

    "scala" in {
      test(("lang", "scala"))(_.contains("case class"), _.contains("`private`"), !_.contains(" private "))
    }

    "java" in {
      test(("lang", "java"))(_.contains("public class"), _.contains("_private"), !_.contains(" private "))
    }
  }
}

object HOCONSpec extends SpecBase {

  "HOCON" should {

    val akkaURL = "https://raw.githubusercontent.com/akka/akka/v2.3.6/"

    val urls: Seq[String] = Seq(
   //   akkaURL + "akka-remote/src/main/resources/reference.conf", // TODO support multi config file
      akkaURL + "akka-actor/src/main/resources/reference.conf"
    )

    def test(param: (String, String)) = {
      forall(List("", "api")) { path =>
        forall(urls) { url =>
          val req = Scalaj(path).params("url" -> url, "hocon" -> "true", "json_library" -> "play", "json_library" -> "argonaut", param)
          req.responseCode must_== 200
          val str = req.asString
          println(str)
          true
        }
      }
    }

    "scala" in {
      test(("lang", "scala"))
    }

    "java" in {
      test(("lang", "java"))
    }
  }
}

