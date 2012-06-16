package com.github.xuwei_k.scalajb

import unfiltered.request._
import unfiltered.response._
import java.net.URL

class Web extends unfiltered.filter.Plan {

  object URL extends Params.Extract(
    "url",
    Params.first ~> Params.nonempty
  )

  object JSON extends Params.Extract(
    "json",
    Params.first ~> Params.nonempty
  )

  def intent = {
    case GET(Path("/api") & Params(URL(url))) => 
      ResponseString(Scalajb.fromURL(url).mkString("\n\n"))
    case GET(Path("/api") & Params(JSON(j))) => 
      ResponseString(Scalajb.fromJSON(j).mkString("\n\n"))
    case GET(Path("/") & Params(URL(url))) => 
      htmlPre(Scalajb.fromURL(url).mkString("\n\n"))
    case GET(Path("/") & Params(JSON(j))) => 
      htmlPre(Scalajb.fromJSON(j).mkString("\n\n"))
  }

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
