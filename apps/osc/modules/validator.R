validateSubmit <- function(session, data, input) {
  if (length(data) == 0) {
      shinyalert("Oops!", "Please, select hints!", type = "error",
                 closeOnClickOutside = TRUE)
      FALSE # silent failure
  } else {
          if ((nrow(data %>% filter_all(any_vars(. == ''))) != 0) & (input$`empty-fields` == FALSE)){ 
              shinyalert("Oops!", "Submission Aborted! Please, fill all input boxes!", type = "error",
                         closeOnClickOutside = TRUE)
              session$sendCustomMessage('fillData', jsonlite::toJSON(data)) # show which select boxes were missed
              FALSE # silent failure
          } else {
              shinyalert("You have successfully submitted:", 'Show which data is added', type = "success",
                         closeOnClickOutside = TRUE
              )
              shinyjs::runjs("$('.error').each(function(){ return $(this).removeClass('error') })") # remove error boxes
              session$sendCustomMessage('submitData', jsonlite::toJSON(data)) # show which data was updated
              NULL
            }
  }
}