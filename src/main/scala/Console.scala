package com.github.xuwei_k.scalajb

import scala.xml.NodeSeq

object Console {
  val value: NodeSeq = {
<html>
  <head>
    <script type="text/javascript" src="//code.jquery.com/jquery-2.1.0.js"></script>
    <script type="text/javascript" src="scalajb.js"></script>
    <title>scala json binding tool</title>
    <link rel="stylesheet" href="/scalajb.css" type="text/css" />
  </head>
  <body>
    <h1>Json binding tool</h1>
    <p>
      <button id='convert'>convert</button>
      <button id='clear_error_message' >clear error message</button>
      <form>
        <input type='radio' name='source_type' id='source_type_json' value='json'>json</input>
        <input type='radio' name='source_type' id='source_type_hocon' value='hocon'>hocon</input>
      </form>
    </p>
    <div class='src_wrap_div'>
      <div>
        <div id="scalajbcode_wrap" class="source_code">
          <textarea id='scalajbcode'></textarea>
        </div>
      </div>
      <div><pre class="source_code prettyprint" id='scalacode'/></div>
    </div>
    <div id='error_message' />
  </body>
</html>
  }

}
