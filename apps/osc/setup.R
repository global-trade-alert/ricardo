# Import osc functions
for(fct in list.files(paste0(path,"apps/osc/functions"), pattern = ".R", full.names=T)){
  source(fct)
}