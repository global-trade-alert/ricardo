# DASHBOARD SERVER FUNCTIONS
settingsserver <- function(input, output, session, user, ...) {
  
  ns <- NS("settings")
  
  # OUTPUT USER INFOS
  observe({
    print("USERACCOUNT: OUTPUT USER INFOS")

    appList = gta_sql_multiple_queries(paste0("SELECT * FROM ric_app_log
                                                WHERE app_id IN (SELECT app_id FROM ric_app_group
                                                  WHERE group_id IN (",paste0(user$group, collapse=", "),"));"),output.queries=1)
    
    appList.output <- tagList(tags$head(tags$script("editDeleteApps();")))
    for(r in 1:nrow(appList)) {
      appList.output <- tagList(appList.output,
                                tags$div(id=paste0("appID-",appList$app.id[r]),
                                         class="app-item",
                                         tags$p(class="name",
                                                paste0(appList$name[r])),
                                           tags$div(class="deleteApp",
                                                    tags$img(src="www/main/delete.svg")),
                                           tags$div(class="editApp",
                                                    tags$img(src="www/main/settings.svg"))),
                                tags$div(id=paste0("editApp-",appList$app.id[r]), class="hidden",
                                         tags$div(class="edit-app-inner")))
    }
    insertUI(selector = "#appList", ui = appList.output)
  })


  
  
}
