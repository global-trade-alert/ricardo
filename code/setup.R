# LOAD PACKAGES
require(shiny)
library(shinyjs)
library(shinyalert)
library(openxlsx)
library(plyr)
library(DT)
library(shinyWidgets)
library(gtalibrary)
library(data.table)
# library(mailR)
library(pool)
library(tidyverse)
library(splitstackshape)
library(gtasql)
library(sodium)
library(stringi)
library(xgboost)

# DEFINE GLOBAL STARTUP VALUES

# source(paste0(path,"code/functions/functions.R"), local = F)
for(fct in list.files(paste0(path,"code/functions/"), pattern = ".R", full.names=T)){
  source(fct)
}

# IMPORT PROMOTION/TRASH CLASSIFIER
for(fct in list.files(paste0(path, "apps/b221/promotion-demotion-classifier/"), pattern = ".R$", full.names=T)){
  source(fct)
}

# IMPORT MODULES
# source(paste0(path,"code/modules/pop_up.R"))

# IMPORT APPS
source(paste0(path, "apps/dashboard/ui.R"))
source(paste0(path, "apps/dashboard/server.R"))
source(paste0(path, "apps/user/ui.R"))
source(paste0(path, "apps/user/server.R"))
source(paste0(path, "apps/settings/ui.R"))
source(paste0(path, "apps/settings/server.R"))
source(paste0(path, "apps/b221/ui.R"))
source(paste0(path, "apps/b221/server.R"))
source(paste0(path, "apps/b221/setup.R"))
source(paste0(path, "apps/osc/ui.R"))
source(paste0(path, "apps/osc/server.R"))
source(paste0(path, "apps/osc/setup.R"))
source(paste0(path, "apps/deliver/ui.R"))
source(paste0(path, "apps/deliver/server.R"))
source(paste0(path, "apps/deliver/setup.R"))
