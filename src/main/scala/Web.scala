package com.github.xuwei_k.scalajb

import unfiltered.request._
import unfiltered.response._

import scala.io.Source
import scalaz.\/

final class Web extends unfiltered.filter.Plan {

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
        _.headOption.collect{
          case "scala" => Lang.SCALA
          case "java"  => Lang.JAVA
        }
      }.getOrElse(Lang.SCALA)
    )
  }

  val DISTINCT = booleanParam("distinct")
  val HOCON = booleanParam("hocon")

  private implicit class CondEither[A](private val value: A) {
    import scalaz.syntax.std.option._
    def condEither[B, C](f: PartialFunction[A, B])(alternative: => C): C \/ B =
      f.lift(value).toRightDisjunction(alternative)
  }

  def intent = {
    case request @ GET(Params(params @ LANG(l) & DISTINCT(d) & HOCON(h) & JSON_LIB(libs))) =>
      request.condEither{
        case Params(JSON(j)) =>
          j
        case Params(URL(url)) =>
          Source.fromURL(url, "UTF-8").mkString
      }(
        ResponseString("you should specify json or url parameter") ~> BadRequest
      ).map{ j =>

        def str = {
          val name = params.get("top_object_name").flatMap(_.headOption.filter(_.nonEmpty))
          val classes = if (h) {
            Scalajb.fromHOCON(j, d, name)
          } else {
            Scalajb.fromJSON(j, d, name)
          }
          classes2string(classes, l, libs)
        }

        PartialFunction.condOpt(request){
          case Path("/api") =>
            ResponseString(str)
          case Path("/") =>
            htmlPre(str)
        }.getOrElse(NotFound)
      }.merge
  }

  def classes2string(classes: Set[CLAZZ], lang: Lang, libs: Set[JsonLib]): String =
    classes.toSeq.sortBy(_.depth).map{ clazz =>
      clazz.str(lang) + JsonLib.objectDef(clazz, libs)
    }.mkString("\n\n")

  def htmlPre(string: String) =
    Html(
      <html>
      <head>
      <style type="text/css"><![CDATA[
        pre{ font-family: Consolas, Menlo, 'Liberation Mono', Courier, monospace;}
      ]]></style>
      </head>
      <body>
        <pre>{string}</pre>
      </body>
      </html>
    )

}
