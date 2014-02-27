package com.github.xuwei_k.scalajb

import scalaz._,Scalaz._
import unfiltered.request._
import unfiltered.response._
import java.net.URL

sealed abstract class Lang
case object JAVA  extends Lang
case object SCALA extends Lang

class Web extends unfiltered.filter.Plan {

  object URL extends Params.Extract(
    "url",
    Params.first ~> Params.nonempty
  )

  object JSON extends Params.Extract(
    "json",
    Params.first ~> Params.nonempty
  )

  object LANG{
    def unapply(p: Params.Map): Option[Lang] = Some(
      p.get("lang").flatMap{
        _.headOption.collect{
          case "scala" => SCALA
          case "java"  => JAVA
        }
      }.getOrElse(SCALA)
    )
  }

  object DISTINCT{
    def unapply(p: Params.Map): Option[Boolean] = Some(
      p.get("distinct").flatMap{
        _.headOption.flatMap{_.parseBoolean.toOption}
      }.getOrElse(false)
    )
  }

  def intent = {
    case GET(Path("/api") & Params(LANG(l) & URL(url) & DISTINCT(d))) =>
      ResponseString(fromURL(url,l,d))
    case GET(Path("/api") & Params(LANG(l) & JSON(j) & DISTINCT(d))) =>
      ResponseString(fromJSON(j,l,d))
    case GET(Path("/") & Params(LANG(l) & URL(url) & DISTINCT(d))) =>
      htmlPre(fromURL(url,l,d))
    case GET(Path("/") & Params(LANG(l) & JSON(j) & DISTINCT(d))) =>
      htmlPre(fromJSON(j,l,d))
  }

  def fromURL(url: String, lang: Lang, distinct: Boolean) =
    Scalajb.fromURL(url, distinct).toSeq.sortBy(_.depth).map(_.str(lang)).mkString("\n\n")

  def fromJSON(j: String, lang: Lang, distinct: Boolean) =
    Scalajb.fromJSON(j, distinct).map(_.str(lang)).mkString("\n\n")

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
