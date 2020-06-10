# APP
rm(list=ls())
gtasql::gta_sql_kill_connections()

# SET PATHS
gta_setwd()
# ----------------------------   set this path on staging environment
# setwd( "~/Dropbox/GTA cloud/")
# path <<- "0 dev/ricardo-lg/"

# ----------------------------   LG local path
settwd("~/Dropbox/")
path <<- "ricardo-lg/" 
# APP SETUP
source(paste0(path,"apps/deliver/setup.R"), local = F)

# LOAD UI AND SERVER
source(paste0(path,"apps/deliver/ui.R"), local = T)
source(paste0(path,"apps/deliver/server.R"), local = F)

# LOAD FUNCTIONS
source(paste0(path,"apps/deliver/functions/retrieve_data.R"))


shinyApp(
  ui = deliverui(id='deliver'),
  server = function(input, output, session) {
            callModule(deliverserver, 'deliver', user = active.user, app=7, prm = list())
          },
  options = list(launch.browser=F, port=4109)
)
