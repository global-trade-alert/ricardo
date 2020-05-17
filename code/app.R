# APP
rm(list=ls())
gtasql::gta_sql_kill_connections()

# SET PATHS
# setwd("/home/rstudio/Dropbox/GTA cloud/")
# setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
# setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")

gta_setwd()
# setwd("/Users/patrickbuess/GTA data team Dropbox/GTA cloud")
# path <<- "17 Shiny/8 ricardo app/"
# path <<- "0 dev/ricardo-lg/"
setwd( "C:/Users/Liubomyr Gavryliv/Dropbox/")
path <<- "ricardo-lg/"

# APP SETUP
source(paste0(path,"code/setup.R"), local = F)

# LOAD UI AND SERVER
source(paste0(path,"code/ui.R"), local = T)
source(paste0(path,"code/server.R"), local = F)

# LOAD FUNCTIONS
source(paste0(path,"code/functions/bt_attribute_hint_processing.R"))

# COOKIE SETTINGS
# sessionid <<- as.character(floor(runif(1)*1e20))
sessionid <<- as.character("123456754321")

# runApp(paste0(path,"code"))
shinyApp(ui = ui,
         server = server,
         onStart = function() {
           gta_sql_kill_connections()
           gta_sql_pool_open(db.title="ricardomainclone",
                             db.host = "gta-ricardo-dev.cp7esvs8xwum.eu-west-1.rds.amazonaws.com",
                             db.name = 'ricardomainclone',
                             db.user = 'gtaricardodev',
                             db.password = 'nC6okGiDKEcFV36rKsykeE9HXbfphgAH6',
                             table.prefix = "ric_")

           onStop(function() {
             cat("Launching application cleanup\n")
             gta_sql_pool_close()
           })
         },
         options = list(launch.browser=T)
)


#LOAD NECESSARY FILES

# shinyApp(ui,
#          server,
#          onStart = function() {
#            gta_sql_pool_open(table.prefix = 'delta_',got.keyring = F)
#            cat("Opening SQL connection\n")
#            onStop(function() {
#              gta_sql_pool_close()
#              cat("Closing SQL connection\n")
#            })
#          }
# )
