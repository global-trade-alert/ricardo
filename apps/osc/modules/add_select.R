addSelectCountry <- function(id){
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tags$div(id=paste0("countrySelect_",id),
           selectInput(id,
                       label = "Change Lead Jurisdiction",
                       choices = '')
  )
}

