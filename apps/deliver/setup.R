# Import deliver functions
for(fct in list.files(paste0(path,"apps/deliver/functions"), pattern = ".R", full.names=T)){
  source(fct)
}


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
