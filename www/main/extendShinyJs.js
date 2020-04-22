  shinyjs.getcookie = function(params) {
    var cookie = Cookies.get("ricardo");
    console.log(cookie);
    if (typeof cookie !== "undefined") {
      Shiny.setInputValue("jscookie", cookie, {priority: "event"});
      console.log(cookie);
    } else {
      var cookie = "";
      Shiny.setInputValue("jscookie", cookie, {priority: "event"});
    }
  }
  shinyjs.setcookie = function(params) {
    Cookies.set("ricardo", escape(params), { expires: 0.5 });  
    Shiny.onInputChange("jscookie", params);
    console.log("cookie set");
  }
  shinyjs.rmcookie = function(params) {
    Cookies.remove("ricardo");
    Shiny.onInputChange("jscookie", "");
  }
