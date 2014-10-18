package com.github.xuwei_k.scalajb

import unfiltered.request._
import unfiltered.response._

import scala.io.Source
import scalaz.\/

final class Web extends unfiltered.filter.Plan {
  import Web._

  def intent = {
    case request @ GET(Params(params @ LANG(l) & DISTINCT(d) & HOCON(h) & JSON_LIB(libs) & IMPLICIT(isImplicit))) =>
      request.condEither{
        case Params(JSON(j)) =>
          j
        case Params(URL(url)) =>
          Source.fromURL(url, "UTF-8").mkString
      }(
        ResponseString("you should specify json or url parameter") ~> BadRequest
      ).map{ j =>

        lazy val result = {
          val name = params.get("top_object_name").flatMap(_.headOption.filter(_.nonEmpty))
          val jsonString = if(h) Scalajb.hocon2jsonString(j) else j
          Scalajb.run(jsonString, name, d, libs, l, isImplicit)
        }

        lazy val status = if(result.isRight) Ok else BadRequest

        PartialFunction.condOpt(request){
          case Path("/api") =>
            ResponseString(result.merge) ~> status
          case Path("/") =>
            htmlPre(result.merge, l) ~> status
        }.getOrElse(NotFound)
      }.merge
  }
}

object Web {

  object URL extends Params.Extract(
    "url",
    Params.first ~> Params.nonempty
  )

  object JSON extends Params.Extract(
    "json",
    Params.first ~> Params.nonempty
  )

  object JSON_LIB extends Params.Extract(
    "json_library", values => Some(values.flatMap(JsonLib.map.get).toSet)
  )

  def booleanParam(key: String, default: Boolean = false) = {
    import scalaz.syntax.std.string._
    new Params.Extract(params => Option(
      params.get(key).flatMap {
        _.headOption.flatMap {
          _.parseBoolean.toOption
        }
      }.getOrElse(default)
    ))
  }

  object LANG{
    def unapply(p: Params.Map): Option[Lang] = Some(
      p.get("lang").flatMap{
        _.headOption.flatMap(Lang.map.get)
      }.getOrElse(Lang.SCALA)
    )
  }

  val DISTINCT = booleanParam("distinct")
  val HOCON = booleanParam("hocon")
  val IMPLICIT = booleanParam("implicit", true)

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
