# IMPORT MODULES
# source(paste0(path,"apps/b221/modules/pop_up.R"))

# Import b221 functions
for(fct in list.files(paste0(path,"apps/b221/functions"), pattern = ".R", full.names=T)){
  source(fct)
}