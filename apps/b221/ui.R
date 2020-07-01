# addResourcePath(prefix = 'www', directoryPath = paste0(path,'apps/b221/www'))

b221ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # START UI
  tagList(
    fluidPage(
    theme = "www/b221/style.css",
    tags$head(
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
                               style="visibility:hidden;",
                               tags$div(id="loadMoreButton",
                                        actionButton(ns("loadMoreLeads"),
                                                     "Refresh",
                                                     class="blue")),
                               tags$div(id="submitButton")),
                      tags$div(id=ns("close-button"),
                               tags$span(class="material-icons",
                                         "close")),
                      tags$div(id=ns("slideInRight"),
                               style="visibility:hidden;",
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
  tags$style("#confirm-discard {opacity:0;visibility:hidden;}"),
  tags$div(id="confirm-discard",
           style="visibility:hidden;",
           tags$div(class="confirm-discard-inner",
                    tags$p("You are discarding a collection"),
                    tags$div(class="discard-select-fields"),
                    tags$div(class="button-wrap",
                             tags$button(class="cancel btn",
                                         "Cancel"),
                             actionButton(ns("discardCollection"),
                                          label="Delete",
                                          icon = icon("trash")))))
  )
  )
  
  
}
