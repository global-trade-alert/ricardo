# Module UI function
settingsui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
  tags$div(class="removeui",
    tags$div(class="settings pad-container",
             tags$div(class="center-console",
                     tags$div(class="panel-grid",
                              tags$div(id="appList")
                      ))
    )
             
  )
  )
}

