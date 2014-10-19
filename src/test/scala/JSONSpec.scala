package com.github.xuwei_k.scalajb

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
          req.responseCode mustEqual 200
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
