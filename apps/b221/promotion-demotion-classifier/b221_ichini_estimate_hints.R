b221_ichini_estimate_hints = function (input.df,
                                       confidence.threshold = 0.2){
  
  library(keras)
  library(dplyr)
  library(purrr)
  library(gtabastiat)
  library(gtalibrary)
  
  
  x_estimate = data.frame(input.df$fl.weighted.average.relevance, 
                      input.df$mrs.hudson.score, 
                      input.df$bastiat.rel.prob) %>% 
    as.matrix()
  
  
  cc_make_prediction = function(input.prediction.vect, conf.threshold){
    
    output.prediction.vect = sapply(input.prediction.vect - conf.threshold, ceiling)
    return(output.prediction.vect)
    
  }
  
  
  current.wd = getwd()
  wd.pref = str_extract(getwd(), ".+(?=GTA data team Dropbox)")
  load.wd = paste0(wd.pref, "GTA data team Dropbox/GTA cloud/0 dev/ricardo-freelancer-editor-classifier/ichini_model.h5")
  
  print(paste("Loading model from", load.wd))
  
  ichini.model = load_model_hdf5(filepath = load.wd)
  
  model.predictions = ichini.model %>% predict_proba(x_estimate)
  
  hard.prediction = cc_make_prediction(model.predictions, 
                                       confidence.threshold) %>%
    sapply(function(x) ifelse(test = (x==1), yes = 1, no = -1))
  
  
  return(hard.prediction)
  
}