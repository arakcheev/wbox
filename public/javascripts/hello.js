if (window.console) {
  console.log("Welcome to your Play application's JavaScript!");
}

$(function(){
  $.ajax({
    type: "POST",
    url: "/documents/update?uuid=idh6c3vjp39dnr9190f1ufkg96",
    data: JSON.stringify({
        name: "doc",
        params: {
          "footer": "idh foo",
          "header": "idh header"
        },
       tags: ["1","2","3","4"]
    }),
    contentType: "application/json"
  })
      .done(function( msg ) {
        console.log( "Data Saved: " + msg );
      });

});