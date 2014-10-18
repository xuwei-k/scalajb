package com.github.xuwei_k.scalajb

import unfiltered.request._
import unfiltered.response._

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
    "json_library", values => Some(values.flatMap(JsonLib.map.get))
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

  def intent = {
    case GET(Path("/api") & Params(LANG(l) & URL(url) & DISTINCT(d) & HOCON(h))) =>
      val result = if(h) fromHOCON_URL(url, l, d) else fromURL(url, l, d)
      ResponseString(result)
    case GET(Path("/api") & Params(LANG(l) & JSON(j) & DISTINCT(d) & HOCON(h))) =>
      val result = if (h) fromHOCON(j, l, d) else fromJSON(j, l, d)
      ResponseString(result)
    case GET(Path("/") & Params(LANG(l) & URL(url) & DISTINCT(d) & HOCON(h))) =>
      val result = if (h) fromHOCON_URL(url, l, d) else fromURL(url, l, d)
      htmlPre(result)
    case GET(Path("/") & Params(LANG(l) & JSON(j) & DISTINCT(d) & HOCON(h))) =>
      val result = if (h) fromHOCON(j, l, d) else fromJSON(j, l, d)
      htmlPre(result)
  }

  def classes2string(classes: Set[CLAZZ], lang: Lang): String =
    classes.toSeq.sortBy(_.depth).map(_.str(lang)).mkString("\n\n")

  def fromURL(url: String, lang: Lang, distinct: Boolean): String =
    classes2string(Scalajb.fromURL(url, distinct), lang)

  def fromHOCON_URL(url: String, lang: Lang, distinct: Boolean): String =
    classes2string(Scalajb.fromHOCON_URL(url, distinct), lang)

  def fromJSON(j: String, lang: Lang, distinct: Boolean): String =
    classes2string(Scalajb.fromJSON(j, distinct), lang)

  def fromHOCON(j: String, lang: Lang, distinct: Boolean): String =
    classes2string(Scalajb.fromHOCON(j, distinct), lang)

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
