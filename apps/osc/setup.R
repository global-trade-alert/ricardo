# IMPORT MODULES
source(paste0(path,"apps/osc/modules/pop_up.R"))

# Import osc functions
for(fct in list.files(paste0(path,"apps/osc/functions"), pattern = ".R", full.names=T)){
  source(fct)
}