package com.github.xuwei_k.scalajb

import argonaut.{PrettyParams, JsonParser, Json}
import scala.io.Source
import scalaj.http.{Http => ScalajHttp, _}

object PostApiSpec extends SpecBase {
  "http post" should {
    "http post" in {
      def post[A](json: Json, expectStatus: Int, success: Boolean, ifSuccess: String => A, ifFailure: String => A) = {
        val req = ScalajHttp.postData("http://localhost:" + port + "/api", json.toString).options(OPTIONS)

        def j(str: String) = JsonParser.parse(str).fold(
          e => {
            println(e)
            failure(e)
          },
          result => {
            result.field("result").flatMap(_.string).map{ a =>
              println(a)
              ifSuccess(a)
            }.getOrElse(
              result.field("error").flatMap(_.string).map{ a =>
                println(a)
                ifFailure(a)
              }.getOrElse(
                failure(result.toString)
              )
            )
            result.field("success") mustEqual Some(Json.jBool(success))
          }
        )

        try{
          val (status, headers, res) = req.asHeadersAndParse(ScalajHttp.readString)
          headers.get("Content-Type") mustEqual Some(List("application/json; charset=utf-8"))
          j(res)
          status mustEqual expectStatus
        }catch{
          case e: HttpException =>
            println(e.message)
            j(e.body)
            e.code mustEqual expectStatus
        }
      }

      val constFalse = (_: String) => false

      post(
        Json.obj(),
        400,
        false,
        constFalse,
        _ mustEqual """(Attempt to decode value on failed cursor.,CursorHistory([*.--\(json)]))"""
      )

      post(
        Json.obj("json" -> Json.jString("{")),
        400,
        false,
        constFalse,
        _ mustEqual "JSON terminates unexpectedly."
      )

      post(
        Json.obj("json" -> Json.jString("{}")),
        200,
        true,
        _ must contain("final case class Unknown("),
        constFalse
      )

      post(
        Json.obj("json" -> Json.jString("{}"), "top_object_name" -> Json.jFalse),
        400,
        false,
        constFalse,
        _ mustEqual "(String,CursorHistory([--\\(top_object_name)]))"
      )

      post(
        Json.obj("json" -> Json.jString("{}"), "top_object_name" -> Json.jString("Hoge")),
        200,
        true,
        _ must contain("final case class Hoge("),
        constFalse
      )

      post(
        Json.obj("json" -> Json.jString("""{ "a" : 1 }"""), "json_library" -> Json.jSingleArray(Json.jString("play"))),
        200,
        true,
        _ must contain(
          """implicit val UnknownFormat: OFormat[Unknown] = (__ \ "a").format[Long].inmap(Unknown.apply _, Function.unlift(Unknown.unapply))"""
        ),
        constFalse
      )

      post(
        Json.obj("json" -> Json.jString("""{ "a" : 1 }"""), "json_library" -> Json.jString("play")),
        200,
        true,
        _ must contain(
          """implicit val UnknownFormat: OFormat[Unknown] = (__ \ "a").format[Long].inmap(Unknown.apply _, Function.unlift(Unknown.unapply))"""
        ),
        constFalse
      )

      post(
        Json.obj("json" -> Json.jString("""{ "b" : [1] }"""), "json_library" -> Json.jSingleArray(Json.jString("argonaut"))),
        200,
        true,
        _ must contain("""CodecJson.casecodec1(apply, unapply)("""),
        constFalse
      )

      val akkaReferenceConf = Source.fromURL(HOCONSpec.akkaActorReferenceConf).mkString

      post(
        Json.obj("json" -> Json.jString(akkaReferenceConf), "hocon" -> Json.jTrue, "json_library" -> Json.jSingleArray(Json.jString("argonaut"))),
        200,
        true,
        _ must contain("""CodecJson.casecodec1(apply, unapply)("""),
        constFalse
      )

    }
  }
}
