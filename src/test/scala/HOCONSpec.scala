package com.github.xuwei_k.scalajb

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
          req.responseCode mustEqual 200
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

