# addResourcePath(prefix = 'www', directoryPath = paste0(path,'apps/b221/www'))

b221ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # START UI
  tagList(
    fluidPage(
    theme = "www/b221/style.css",
    tags$head(
      tags$script(src="www/b221/app.js"),
      tags$script(src="http://cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.3.0/js/bootstrap-datepicker.js"),
      tags$link(rel="stylesheet", type="text/css", href="https://cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.9.0/css/bootstrap-datepicker.min.css")
    ),
    tags$div(class="removeui",
             tags$div(class="wrap b221",
                      tags$div(class="content",
                               tags$div(class="leadsTable",
                                        dataTableOutput(ns("leadsTable")),
                                        tags$div(class='remove',selectizeInput('remove', label=NULL, choices=NULL, width=10),
                                                 dateInput("date4", "Date:"),
                                                 style="visibility:hidden; pointer-events: none;")) # Initialize selectize),
                      ),
                      tags$div(class="control-bar",
                               tags$div(id="loadMoreButton",
                                        actionButton(ns("loadMoreLeads"),
                                                     "Refresh",
                                                     class="blue")),
                               tags$div(id="submitButton")),
                      tags$div(id=ns("close-button"),
                               tags$span(class="material-icons",
                                         "close")),
                      tags$div(id=ns("slideInRight"),
                               tags$div(class="outer-wrap",
                                        tags$div(id=paste0("hintSlideIn"),
                                                 class="inner-wrap",
                                                 tags$div(id = "collectionValues"),
                                                 tags$div(class="lower-tables",
                                                          tags$div(class="collectionTable",
                                                                   tags$h3("Add to existing collection"),
                                                                   dataTableOutput(ns("collectionTable"))),
                                                          tags$div(class="singleHintTable",
                                                                   tags$h3("Add hints to active collection"),
                                                                   dataTableOutput(ns("singleHintsTable")))
                                                          
                                                 )
                                        )
                                        )
             )
             )
  ),
  tags$div(id="confirm-discard",
           tags$div(class="confirm-discard-inner",
                    tags$p("You are deleting a collection"),
                    tags$div(class="button-wrap",
                             tags$button(class="cancel btn",
                                         "Cancel"),
                             actionButton(ns("discardCollection"),
                                          label="Delete",
                                          icon = icon("times")))))
  )
  )
  
  
}
