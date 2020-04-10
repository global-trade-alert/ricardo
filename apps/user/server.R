# DASHBOARD SERVER FUNCTIONS
userserver <- function(input, output, session, user, prm, ...) {
  
  ns <- NS("user")
  
  print("USERACCOUNT: SUCCESS")
  print(gta_sql_get_value(sqlInterpolate(pool, "SELECT f_name FROM gta_user_log WHERE user_id = ?userLogin;", userLogin = user$id)))
  
  # OUTPUT USER INFOS
  observe({
    print("USERACCOUNT: OUTPUT USER INFOS")
    
    firstName = gta_sql_get_value(sqlInterpolate(pool, "SELECT f_name FROM gta_user_log WHERE user_id = ?userLogin;", userLogin = user$id))
    lastName = gta_sql_get_value(sqlInterpolate(pool, "SELECT l_name FROM gta_user_log WHERE user_id = ?userLogin;", userLogin = user$id))
    userName = gta_sql_get_value(sqlInterpolate(pool, "SELECT user_login FROM gta_user_log WHERE user_id = ?userLogin;", userLogin = user$id))
    mailAddress = gta_sql_get_value(sqlInterpolate(pool, "SELECT user_email FROM gta_user_log WHERE user_id = ?userLogin;", userLogin = user$id))
    
    credentials.output <- tagList(
      tags$div(class="user-credentials",
        textInput(ns("credFirstName"), label="First Name", value = firstName),
        textInput(ns("credLastName"), label="Last Name", value = lastName),
        textInput(ns("credUserName"), label="Username", value = userName),
        textInput(ns("credMailAddress"), label="Mail address", value = mailAddress),
        actionButton(inputId = ns("credUpdate"), class="submitButton", label="Save changes")
        ),
      tags$div(class="user-password",
        passwordInput(ns("credPassword"),label="Password", value = "This is no password"),
        actionButton(inputId = ns("pwdUpdate"), class="submitButton", label="Change password")
      )
    )
    insertUI(selector = "#credPlaceholder", ui = credentials.output)
  })

  observeEvent(input$credUpdate, {
    result.vector <- c(input$credFirstName, input$credLastName, input$credUserName, input$credMailAddress)
    gta_sql_update_table(sqlInterpolate(pool, "UPDATE gta_user_log SET f_name = ?credFirstName, l_name = ?credLastName, user_login = ?credUserName, user_email = ?credMailAddress WHERE user_id = ?userID;", 
                                        credFirstName = input$credFirstName,
                                        credLastName = input$credLastName,
                                        credUserName = input$credUserName,
                                        credMailAddress = input$credMailAddress,
                                        userID = user$id))
    showNotification("Your changes have been saved.", duration = 3)
  })  
  
  observeEvent(input$pwdUpdate, {
    gta_sql_update_table(sqlInterpolate(pool, "UPDATE gta_user_log SET password = ?credPassword WHERE user_id = ?userID;", 
                                        credPassword = password_store(input$credPassword),
                                        userID = user$id))
    showNotification("Your password has been changed.", duration = 3)
  })
  
}
