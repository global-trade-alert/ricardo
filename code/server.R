# SERVER
server <- function(input, output, session) {
  
  # UNCOMMENT FOR TESTING, PREVENTS LOGIN CONSOLE FROM BEING SHOWN
  # @JF: COMMENT OUT THIS OBSERVE STATEMENT
  # observe({
  #   active.user$id <- 40
  #   active.user$name <- "Patrick Buess"
  #   active.user$group <- c(1,2)
  #   active.user$loggedIn <- TRUE
  #   print(paste0("USER LOGGED IN: ", active.user$name))
  #   removeClass(class = "inactive", selector = "#placeholderUI")
  # })
  
  # Set encoding for all tables, to make filtering in DT work
  gta_sql_get_value('SET NAMES utf8;')
  
  
  observe({
    js$getcookie()
  })

  # check if a cookie is present and matching our super random sessionid 
  loggedIn <- reactiveVal(FALSE) 
  active.menu.item <- reactiveVal()
  active.user <<- reactiveValues(id = NULL, name = NULL, loggedIn = FALSE, group=NULL)
  active.user.selected <- reactiveVal(0)
  active.hash <- reactiveVal()
  active.app <- reactiveVal(0)
  app.log <<- reactive({
    if (loggedIn()) {
      app.log <- gta_sql_multiple_queries(paste0("SELECT * FROM ric_app_log 
                                            WHERE app_id IN (SELECT app_id FROM ric_app_group 
                                                WHERE group_id IN (",paste0(active.user$group, collapse=", "),"));"), output.queries = 1)
      app.log$slug <- paste0(clean(trimws(app.log$name,which = "both")),app.log$app.id)
      app.log <- app.log
    }
  })

  observeEvent(input$jscookie, {
    cat("\n\n")
    print("GET COOKIE")
    print(input$jscookie)
    print(sessionid)
    
    if (nchar(input$jscookie)>0 & sessionid == strsplit(input$jscookie, "-")[[1]][1]) {
      print("USER ID")
      user.id = print(strsplit(input$jscookie, "-")[[1]][2])
      user.credentials <- gta_sql_get_value(sqlInterpolate(pool, "SELECT * FROM gta_user_log WHERE user_id = ?userID;", userID = user.id))
      active.user$id <- user.id
      active.user$name <- paste0(user.credentials$f.name," ",user.credentials$l.name)
      active.user$group <- gta_sql_multiple_queries(paste0("SELECT group_id FROM gta_user_group WHERE user_id = ",active.user$id,";"), output.queries = 1)
      active.user$loggedIn <- TRUE
      active.user.selected <- active.user.selected(active.user$id)

      loggedIn(TRUE)

    }
    else {
        loggedIn(FALSE)
        addClass(class = "inactive", selector = "#placeholderUI")
        showModal(modalDialog(
          title = NULL,
          textInput('userInp', label = NULL, placeholder = "Login Name"),
          passwordInput('pwInp', label = NULL, placeholder = 'Password'),
          actionButton('submitLogin', 'Login', class = 'btn action-button btn-success submitButton', icon = icon('sign-in')),
          size = 's',
          easyClose = FALSE,
          footer = NULL
        ))
          }
  })

  
  # APP TYPES
  app.list <- gta_sql_load_table(load.table = "app.list")

  observe({
    print("LOGGEND IN")
    print(loggedIn())
  })

  # UPDATE DATE OF CREATION OF APP.R WHEN CLOSING, PREVENTS CACHING OF CSS AND JS
  onStop(function() {
    p <- paste0(getwd(), path, "/code/app.R")
    # Update file 'date creation'
    Sys.setFileTime(p, Sys.time())
  })
  

  
  # SUBMIT LOGIN CREDENTIALS
  observeEvent(input$submitLogin, {
    print(paste0("SUBMIT LOGIN"))
    # req(input$userInp, input$pwInp)  ## ensure we have inputs
    user.credentials <- gta_sql_get_value(sqlInterpolate(pool, "SELECT * FROM gta_user_log WHERE user_login = ?userLogin;", userLogin = input$userInp))
    
    if (nrow(user.credentials)==0) {
      showNotification("Username not found", duration = 3)
    } else {
      if (is.na(user.credentials$password) | user.credentials$password == "") {
        gta_sql_update_table(sqlInterpolate(pool, "UPDATE gta_user_log SET password = ?pwd WHERE user_login = ?userLogin;", pwd = password_store(input$pwInp), userLogin = input$userInp))
        removeModal()
      } else {
        if (password_verify(user.credentials$password, input$pwInp)==F) {
          showNotification("Incorrect Password", duration = 3)
        } else {
          active.user$id <- user.credentials$user.id
          active.user$name <- paste0(user.credentials$f.name," ",user.credentials$l.name)
          active.user$group <- gta_sql_multiple_queries(paste0("SELECT group_id FROM gta_user_group WHERE user_id = ",active.user$id,";"), output.queries = 1)
          active.user$loggedIn <- TRUE
          active.user.selected <- active.user.selected(active.user$id)
          removeModal()
          loggedIn <- loggedIn(TRUE)
          print(paste0("USER LOGGED IN: ", active.user$name))
          # js$rmcookie()
          js$setcookie(paste(sessionid, active.user$id, sep = "-"))
          removeClass(class = "inactive", selector = "#placeholderUI")
        }
      }
    }
  })

  
  # SET ACTIVE MENU ITEM ACCORDING TO HASH
  observe({
    print("SET ACTIVE MENU ITEM")
    active.hash <- active.hash(clean(getUrlHash()))
    if (active.hash() %in% c(app.log()$slug,"dashboard1","user2","settings3")) {
      active.menu.item <- active.menu.item(app.log()$name[app.log()$slug == active.hash()])
      active.app <- active.app(as.numeric(app.log()$app.id[app.log()$slug == active.hash()]))
    } else {
      active.menu.item <- active.menu.item("Dashboard")
      active.app <- active.app(1)
    }
  })

  # DYNAMICALLY CREATE THE MENU OUTPUT
  output$mainMenuOutput <- renderUI({
    if (loggedIn()) {
    print("DYNAMICALLY CREATE THE MENU")
      runjs("console.log('I AM RUNNING 6');")
    menu.html <- character()
    for(m in 1:nrow(app.log())) {
      if (app.log()$app.id[m] > 3) {
      menu.html <- paste0(menu.html,"<a href='#",clean(app.log()$name[m]),app.log()$app.id[m],"' class='menu-item'><p>",app.log()$name[m],"</p><div class='icon'><img class='svg' src='www/main/search.svg'></div></a>")
      }
    }
    HTML(menu.html)
    }
    })

  # OUTPUT THE CURRENTLY ACTIVE MENU ITEM
  output$activeMenuItem <- renderUI({
    
    active.item <- paste0("<div class='active menu-item'>",active.menu.item(),"</div>")
    HTML(active.item)
  })

  # CONDITIONAL CONTENT AREA OUTPUT, DEPENDING ON MENU CHOICE
  observe({
    if (loggedIn()){
    print("DYNAMICALLY OUTPUT THE CONDITIONAL UI")
    shinyjs::runjs(paste0("unbindAll();"))
    removeUI(selector = ".removeui", immediate = T)
    # Get Parameters
    print(active.app())

    if (active.app() > 3) {
      getUI <- clean(app.list$app.name[app.list$type.id == app.log()$type.id[app.log()$app.id == active.app()]])
      appPara.temp <- gta_sql_multiple_queries(paste0("SELECT * FROM ric_app_settings WHERE app_id = ",active.app(),";"),
                                        output.queries = 1)
      appPara <- list()
      for (r in 1:nrow(appPara.temp)) {
        appPara[[appPara.temp$parameter.name[r]]] <- appPara.temp$parameter.value[r]
      }
      rm(appPara.temp)
      
      } else {
        getUI <- clean(app.list$app.name[app.list$type.id == app.log()$type.id[app.log()$app.id == active.app()]])
        appPara <- list()
    }

    # Call ui and server
    eval(parse(text=paste0("condUI <- ",getUI,"ui(id='",getUI,"')")))
    print(paste0("INSERT UI: ", getUI))
    eval(parse(text=paste0("callModule(",getUI,"server, '",getUI,"',user=active.user, app=active.app(), prm = appPara)")))
    insertUI(selector = "#placeholderUI", ui = condUI)
    shinyjs::runjs(paste0("bindAll();"))
  }
  })

  session$allowReconnect(FALSE)

  
}

