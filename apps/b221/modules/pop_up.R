# Module UI function
popUpModuleUI <- function(id, button.label = NULL, module.tags=NULL) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tags$div(class="pop-up-wrap",
           actionButton(gsub("-","",ns("")),
                        button.label),
           tags$div(class=paste0(ns("pop-up")," pop-up-bg"),
                    tags$div(class="pop-up-close-button", tags$img(src="www/closebutton.svg")),
                     tags$div(class="pop-up-inner",
                     module.tags,
                     
                     tags$style(HTML(paste0(".",ns('pop-up')," {visibility:hidden;opacity:0;}
                     .",ns('pop-up'),".active {visibility:visible;opacity:1;}")))
           )))
}