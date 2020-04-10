# UI
# gta_sql_pool_open(table.prefix = 'ric')

addResourcePath(prefix = 'www', directoryPath = paste0(path,'www'))

ui <- fluidPage(
  # START UI
  useShinyjs(),
  theme = "www/main/style.css",
  tags$div(class="wrap",
           tags$div(class="main-menu-wrap",
                     tags$div(id="nav", class="main-menu",
                              tags$div(class="main-menu-upper",
                                        tags$div(class="logo",
                                                 tags$img(src="www/main/gta logo-blue.svg"))
                                       ),
                                tags$div(class="nav-items",
                                         tags$div(class="item dashboard-icon",
                                                  tags$a(href="#dashboard1",
                                                         tags$p("Dashboard"),
                                                         tags$div(class="icon",
                                                                  tags$img(src="www/main/dashboard.svg", class="svg"))
                                                         )),
                                         uiOutput("mainMenuOutput")
                               ),
                              tags$div(class="nav-items-bottom",
                                       tags$div(class="item settings-icon",
                                                tags$a(href="#settings3",
                                                       tags$p("Settings"),
                                                       tags$div(class="icon",
                                                                tags$img(src="www/main/settings.svg", class="svg"))
                                                )),
                                       tags$div(class="item user-account-icon",
                                                tags$a(href="#user2",
                                                       tags$p("Account"),
                                                       tags$div(class="icon",
                                                                tags$img(src="www/main/person.svg", class="svg"))
                                                ))
                                       )
                     )
                    ),
           tags$div(class="header",
                    tags$div(class="menu-toggle left",
                             tags$img(src="www/main/menu.svg"),
                             tags$div(class="active-menu-item",
                                      uiOutput("activeMenuItem")
                             ))),
           tags$div(class="content",
                    tags$div(id="placeholderUI")
                    # tags$div(class="login-panel",
                    #          textInput("username",
                    #                    "Username",
                    #                    placeholder = "Username"),
                    #          actionButton("submitLogin",
                    #                       "Login"))
           ),
           tags$div(class="backdrop-nav")
  ),
          
  
  
  
  
  # ADD JS
  tags$head(
    tags$script(src="www/main/app.js"),
    tags$script(src="www/main/js.cookie.js")
  )
  # tags$script(src="www/b221/app.js")
  
  )
