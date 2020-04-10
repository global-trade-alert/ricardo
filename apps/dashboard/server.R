# Define reactive values used in this app module up here, these will be passed automatically
# Don't forget to add these variables as function parameters as well

# DASHBOARD SERVER FUNCTIONS
dashboardserver <- function(input, output, session, user, prm, ...) {
  
  print("Success server dashboard")
  # DYNAMICALLY APP PANELS FOR DASHBOARD
  output$appPanelsOutput <- renderUI({
    print("DASHBOARD: RENDER UI")
    list.output <- character()
    for(m in 1:nrow(app.log())) {
      if (app.log()$app.id[m] > 3) {
      list.output <- paste0(list.output,"<div class='app-panel-wrap'><div class='app-panel-item'><a href='#",clean(app.log()$name[m]),app.log()$app.id[m],"' class='app-panel-item'><h3 class='title'>",app.log()$name[m],"</h3></a></div><div class='content'></div></div>")
      }
      }
    HTML(list.output)
  })
  
}