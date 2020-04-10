# addResourcePath(prefix = 'www', directoryPath = paste0(path,'apps/b221/www'))

b221ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # START UI
  tagList(
    fluidPage(
    theme = "www/b221/style.css",
    tags$head(
      tags$script(src="www/b221/app.js")
    ),
    tags$div(class="removeui",
             tags$div(class="wrap b221",
                      tags$div(class="content",
                               tags$div(class="leadsTable",
                                        dataTableOutput(ns("leadsTable"))),
                               tags$div(class="loadMoreListener")
                      ),
                      tags$div(class="control-bar",
                               tags$div(id="loadMoreButton"),
                               tags$div(id="submitButton")),
                      tags$div(id="slideInRight",
                               tags$div(class="removeslideinui"))
             )
             )
  )
  )
  
  
}
