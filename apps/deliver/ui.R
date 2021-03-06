deliverui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # START UI
  tagList(
    fluidPage(
      shinyjs::useShinyjs(),
      theme = "www/deliver/style.css",
      tags$head(
      ),
    tags$div(class="removeui",
             tags$div(class='remove', dateInput('remove_date', label=NULL),
                      selectizeInput('remove_input', label=NULL, choices=NULL, width=10)),
             tags$div(class="wrap deliver",
                      tags$div(class="content",
                               tags$div(class="deliverTable",
                                        dataTableOutput(ns("deliverTable"))) # Initialize selectize),
                      ),
                      tags$div(class="control-bar")

             ),
             tags$div(class="overlay"),
             tags$div(class="editMode",
                      tags$div(class="editMode-header",
                               tags$h1('Edit Mode')),
                      tags$div(class="canvas")),
             tags$div(class="saveMode",
                      style = 'visibility: hidden;',
                      tags$div(class="saveMode-header",
                               tags$h1('Save Mode'),
                               tags$button(type="button",
                                           id="save-xlsx",
                                           tags$img(src="www/deliver/save.svg",
                                                    style="margin-right:10px;"),
                                           "Save .xslx")),
                      tags$div(class='save-cols')),
             tags$div(style='visibility:hidden;',
                      downloadButton(ns("downloadXlsx"), "Download")),
             dateInput(ns('lastDeliverable'),value = lubridate::floor_date(lubridate::today()-1, "week", 5), label="")
             
  )
  )
  )
  
  
}