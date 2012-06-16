package com.github.xuwei_k.scalajb

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
    def unapply(p:Params.Map):Option[Lang] = Some(
      p.get("lang").flatMap{
        _.headOption.collect{
          case "scala" => SCALA
          case "java"  => JAVA
        }
      }.getOrElse(SCALA)
    )
  }

  def intent = {
    case GET(Path("/api") & Params(LANG(l) & URL(url))) =>
      ResponseString(fromURL(url,l))
    case GET(Path("/api") & Params(LANG(l) & JSON(j))) =>
      ResponseString(fromJSON(j,l))
    case GET(Path("/") & Params(LANG(l) & URL(url))) =>
      htmlPre(fromURL(url,l))
    case GET(Path("/") & Params(LANG(l) & JSON(j))) =>
      htmlPre(fromJSON(j,l))
  }

  def fromURL(url:String,lang:Lang) =
    Scalajb.fromURL(url).map(_.str(lang)).mkString("\n\n")

  def fromJSON(j:String,lang:Lang) =
    Scalajb.fromJSON(j).map(_.str(lang)).mkString("\n\n")

  def htmlPre(string:String) =
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
