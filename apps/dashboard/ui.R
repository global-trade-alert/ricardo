# Module UI function
dashboardui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
  tags$div(class="removeui",
    tags$div(class="dashboard pad-container",
             tags$div(class="panel-grid",
                       uiOutput(ns("appPanelsOutput"))
              )
    )
             
  )
  )
}

