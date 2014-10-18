$(function(){

  $("#clear_scalacode").click(
    function(){
      $("#scalacode").children().remove();
    }
  );

  $("#clear_error_message").click(
    function(){
      $("#error_message").children().remove();
    }
  );

  $("#convert").click(function(){
      console.log("click!!");

      var sourceContent = $("#scalajbcode").val();

      console.log(sourceContent);

      $("#error_message").children().remove();
      $("#scalacode").children().remove();

      var hocon = ($("input[name='source_type']:checked").val() == "hocon") ? "true" : "false";

      console.log("hocon = " + hocon);

      var url = "/api?json=" + sourceContent + "&hocon=" + hocon;

      console.log(url);

      jQuery.get(
        url,
        {},
        function(data){
          console.log(data);
          $("#scalacode").text(data);
          //prettyPrint();
        }
      ).fail(function(e){
        console.log("error !");
        console.log(e);
        $("#error_message").append("<pre>" + JSON.stringify(e) + "</pre>")
      });
  });
});

$(document).ready(function(){
  $("#source_type_json").attr('checked','checked');
  $("#scalajbcode").val('{"a": 1, "b": ["c", "d"], "e": false}');
  $('#convert').click();
});
