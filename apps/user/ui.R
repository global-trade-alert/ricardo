# Module UI function
userui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
  tags$div(class="removeui",
    tags$div(class="useraccount pad-container",
             tags$div(class="center-console",
                     tags$div(class="panel-grid",
                              tags$div(id="credPlaceholder")
                      ))
    )
             
  )
  )
}

