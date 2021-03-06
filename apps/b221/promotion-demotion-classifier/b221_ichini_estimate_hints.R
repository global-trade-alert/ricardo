b221_ichini_estimate_hints = function (input.df,
                                       confidence.threshold = 0.2,
                                       model.folder = ""){
  print('ichiniestimates')
  library(tensorflow)
  library(keras)
  library(dplyr)
  library(purrr)
  library(gtabastiat)
  library(gtalibrary)
  #library(h5py) #hdf5 model saving/loading is handled by tensorflow/keras
  
  x_estimate = data.frame(input.df$fl.weighted.average.relevance, 
                      input.df$mrs.hudson.score, 
                      input.df$bastiat.rel.prob) %>% 
    as.matrix()
  
  
  cc_make_prediction = function(input.prediction.vect, conf.threshold){
    
    output.prediction.vect = sapply(input.prediction.vect - conf.threshold, ceiling)
    return(output.prediction.vect)
    
  }
  
  #model.folder = "C:/Users/c-cam/GTA data team Dropbox/GTA cloud/0 dev/ricardo-freelancer-checks/apps/b221/promotion-demotion-classifier"
  #current.wd = getwd()
  
  #apps/b221/promotion-demotion-classifier/"
  wd.pref = str_extract(getwd(), ".+GTA data team Dropbox")
  load.wd = paste0(wd.pref, "/GTA cloud/0 dev/ricardo-freelancer-checks/apps/b221/promotion-demotion-classifier/ichini_model.h5")
  
  print(paste("Loading model from", load.wd))
  
  ichini.model = load_model_hdf5(filepath = load.wd)
  
  model.predictions = ichini.model %>% predict_proba(x_estimate)
  
  hard.prediction = cc_make_prediction(model.predictions, 
                                       confidence.threshold) %>%
    sapply(function(x) ifelse(test = (x==1), 
                              yes = 1, 
                              no = -1))
  
  
  return(hard.prediction)
  
}