# Import deliver functions
for(fct in list.files(paste0(path,"apps/deliver/functions"), pattern = ".R", full.names=T)){
  source(fct)
}
