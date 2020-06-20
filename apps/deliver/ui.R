# addResourcePath(prefix = 'www', directoryPath = paste0(path,'apps/b221/www'))

 addResourcePath(prefix = 'www', directoryPath = paste0(path,'www/deliver'))

deliverui <- function(id) {

  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # START UI
  tagList(
    fluidPage(
      theme = "www/deliver/style.css",
      tags$head(
        tags$script(src="www/deliver/app.js"),
        tags$script(src="http://cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.3.0/js/bootstrap-datepicker.js"),
        tags$link(rel="stylesheet", type="text/css", href="https://cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.9.0/css/bootstrap-datepicker.min.css")
      ),
    tags$div(class="removeui",
             tags$div(class='remove', dateInput('remove_date', label=NULL)),
             tags$div(class="wrap deliver",
                      tags$div(class="content",
                               tags$div(class="deliverTable",
                                        dataTableOutput(ns("deliverTable"))) # Initialize selectize),
                      ),
                      tags$div(id="searchPanes"),
                      tags$div(class="control-bar")

             )
  )
  )
  )
  
  
}
