# APP
rm(list=ls())
gtasql::gta_sql_kill_connections()

# SET PATHS
gta_setwd()
# ----------------------------   set this path on staging environment
setwd( "~/Dropbox/GTA cloud/")
path <<- "0 dev/ricardo-lg/"

# ----------------------------   LG local path
# setwd("~/Dropbox/")
# path <<- "ricardo-lg/" 
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
  onStart = function() {
    gta_sql_kill_connections()
    gta_sql_pool_open(db.title="ricardomainclone",
                      db.host = "gta-ricardo-dev.cp7esvs8xwum.eu-west-1.rds.amazonaws.com",
                      db.name = 'ricardomainclone',
                      db.user = 'gtaricardodev',
                      db.password = 'nC6okGiDKEcFV36rKsykeE9HXbfphgAH6',
                      table.prefix = "bt_")
    
    onStop(function() {
      cat("Launching application cleanup\n")
      gta_sql_pool_close()
    })
  },
  options = list(launch.browser=T, port=4109)
)
