# addResourcePath(prefix = 'www', directoryPath = paste0(path,'apps/b221/www'))

 addResourcePath(prefix = 'www', directoryPath = paste0(path,'www/deliver'))

deliverui <- function(id) {

  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # START UI
  tagList(
    fluidPage(
    theme = "www/style.css",
    tags$head(
      tags$script(src="www/app.js")
    ),
    tags$div(class="removeui",
             tags$div(class="wrap deliver",
                      tags$div(class="content",
                               tags$div(class="deliverTable",
                                        dataTableOutput(ns("deliverTable"))) # Initialize selectize),
                      ),
                      #tags$div(class='remove',selectizeInput('remove', label=NULL, choices=NULL, width=10)),
                      tags$div(class="control-bar",
                               tags$div(id="loadMoreButton",
                                        actionButton(ns("loadMoreDeliver"),
                                                     "Refresh",
                                                     class="blue")))

             )
  )
  )
  )
  
  
}
