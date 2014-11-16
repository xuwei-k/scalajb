package com.github.xuwei_k.scalajb

import argonaut.{JsonParser, Json}
import unfiltered.filter.Plan.Intent
import scala.io.Source
import scalaz.{\/-, \/}
import unfiltered.request._
import unfiltered.response._

final class Web extends unfiltered.filter.Plan {
  import Web._

  private[this] final val SCALAJB_JS = "/scalajb.js"

  private[this] def fromResource(name: String): ResponseString = ResponseString(
    scala.io.Source.fromInputStream(getClass.getResourceAsStream(name))("UTF-8").mkString
  )

  def intent: Intent = ({
    case GET(Path("/console")) =>
      HtmlContent ~> fromResource("/console.html")

    case GET(Path(SCALAJB_JS)) =>
      JsContent ~> fromResource(SCALAJB_JS)

    case request @ GET((Path("/api") | Path("/")) & Params(params @ LANG(l) & Param.DISTINCT(d) & Param.HOCON(h) & JSON_LIB(libs) & Param.IMPLICIT(isImplicit) & Param.COMMENT(comment))) =>
      request.condEither{
        case Params(JSON(j)) =>
          j
        case Params(URL(url)) =>
          Source.fromURL(url, "UTF-8").mkString
      }(
        ResponseString("you should specify " + Param.JSON + " or url parameter") ~> BadRequest
      ).map{ j =>
        lazy val result = {
          val name = params.get(Param.TOP_OBJECT_NAME).flatMap(_.headOption.filter(_.nonEmpty))
          for{
            jsonString <- if(h) {
              Scalajb.hocon2jsonString(j).leftMap(error =>
                error.toString + error.getStackTrace.mkString("\n\n", "\n", "")
              )
            } else \/-(j)
            * <- Scalajb.run(jsonString, name, d, libs, l, isImplicit, comment)
          } yield *
        }

        lazy val status = if(result.isRight) Ok else BadRequest

        PartialFunction.condOpt(request){
          case Path("/api") =>
            ResponseString(result.merge) ~> status
          case Path("/") =>
            htmlPre(result.merge, l) ~> status
        }.getOrElse(NotFound)
      }.merge
    case request @ POST(Path("/api")) =>

      val SUCCESS = "success"

      JsonContent ~> (for{
        paramJson <- JsonParser.parse(Body.string(request))
        param <- paramJson.as[Param].toDisjunction.leftMap(_.toString())
        sourceJson <- param.json
        result <- Scalajb.run(
          sourceJson, param.topObjectName, param.distinct, param.jsonLibrary, param.lang, param.isImplicit, param.comment
        )
      } yield {
        ResponseString(
          Json.obj(
            SUCCESS -> Json.jTrue,
            "result" -> Json.jString(result)
          ).toString
        )
      }).leftMap( error =>
        BadRequest ~> ResponseString(
          Json.obj(
            SUCCESS -> Json.jFalse,
            "error" -> Json.jString(error)
          ).toString
        )
      ).merge
  }: Intent).orElse(TwentyThree.intent)
}

object Web {

  object URL extends Params.Extract(
    "url",
    Params.first ~> Params.nonempty
  )

  object JSON extends Params.Extract(
    Param.JSON,
    Params.first ~> Params.nonempty
  )

  val JSON_LIB: NonEmptyExtractor[Set[JsonLib]] =
    NonEmptyExtractor(
      _.getOrElse(Param.JSON_LIBRARY, Nil).flatMap(JsonLib.map.get).toSet
    )

  def booleanParam(key: String, default: Boolean): NonEmptyExtractor[Boolean] = {
    import scalaz.syntax.std.string._
    NonEmptyExtractor(
      _.getOrElse(key, Nil).flatMap(
        _.parseBoolean.toOption
      ).headOption.getOrElse(default)
    )
  }

  val LANG: NonEmptyExtractor[Lang] =
    NonEmptyExtractor(
      _.get(Param.LANG).flatMap{
        _.headOption.flatMap(Lang.map.get)
      }.getOrElse(Lang.SCALA)
    )

  private implicit class CondEither[A](private val value: A) {
    import scalaz.syntax.std.option._
    def condEither[B, C](f: PartialFunction[A, B])(alternative: => C): C \/ B =
      f.lift(value).toRightDisjunction(alternative)
  }

  private[this] final val highlightJsURL = "//cdnjs.cloudflare.com/ajax/libs/highlight.js/8.3/"

  def htmlPre(string: String, lang: Lang) =
    Html(
      <html>
      <head>
      <meta name="robots" content="noindex,nofollow" />
      <script type="application/javascript" src={highlightJsURL + "highlight.min.js"}></script>
      <link rel="stylesheet" href={highlightJsURL + "styles/github.min.css"} />
      <script>hljs.initHighlightingOnLoad();</script>
      </head>
      <body>
        <pre><code style="font-family: Consolas, Menlo, 'Liberation Mono', Courier, monospace;" class={lang.name}>{string}</code></pre>
      </body>
      </html>
    )

}
