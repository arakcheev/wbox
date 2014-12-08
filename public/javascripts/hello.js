if (window.console) {
    console.log("Welcome to your Play application's JavaScript!");
}

$(function () {
    $.ajax({
        type: "POST"
        , url: "/repositories/invite"
        , beforeSend: function (xhr) {
            xhr.setRequestHeader('X-Repository', 'hq539ov8fl0b0jo7can9hmb7ev');
        }
        , data: JSON.stringify({
            to: "54664063920000f600b2c23e"
            ,rule: 2
        })
        , contentType: "application/json"
    })
        .done(function (msg) {
            console.log("Data Saved: " + msg);
        });

});