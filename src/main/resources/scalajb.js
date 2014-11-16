$(document).ready(function(){

  hljs.initHighlightingOnLoad();

  $("#compile").click(function(){
    console.log("click!!");
    var sample_json = $("#sample_json").val();

    console.log(sample_json);

    var sendData = {
      json_library: ["argonaut", "play"],
      json: sample_json
    };

    var clearAll = function(){
      $("#error_message").children().remove();
      $("#result").text("");
    }

    $.post(
      '/api',
      JSON.stringify(sendData),
      function(data){
        clearAll();
        console.log(JSON.stringify(data, null, "  "));
        if(data.success){
          $("#result").text(data.result);
          $('pre code').each(function(i, block) {
            hljs.highlightBlock(block);
          });
        }
      },
      "JSON"
    ).fail(function(e){
      console.log(e);
      var error = JSON.parse(e.responseText);
      console.log(error);
      clearAll();
      $("#error_message").append("<p style='color: red;'>" + error.error + "</p>");
    });
  });

});
