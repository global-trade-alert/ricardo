b221_ichini_train_model = function (leads.core) {
  
  
  library(keras)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(gtasql)
  library(gtabastiat)
  library(gtalibrary)
  library(xml2)
  library(caret)
  library(gtasql)
  library(pool)
  library(RMariaDB)
  library(tidytext)
  
  
  training.b221.full = leads.core
  
  #random ids to separate into training/testing sets
  #ratio can be changed if necessary
  #currently 90/10. ideally would be 80/20 but we need more data.
  training_id <- sample.int(nrow(training.b221.full), size = nrow(training.b221.full)*0.9)
  training.b221 <- training.b221.full[training_id,]
  testing.b221 <- training.b221.full[-training_id,]
  
  #show summary of the text
  
  training.b221.full$acting.agency %>% 
    strsplit(" ") %>% 
    sapply(length) %>% 
    summary()
  
  #parameters
  num_words = 15000 #vocab size (def = 10k)
  max_length = 3 #because three features
  
  
  #####
  
  
  #####
  
  
  ###method 2
  
  
  #generate the training data.
  #x is the three features. 
  #y is the result, i.e. evaluation
  x_train = data.frame(training.b221$fl.weighted.average.relevance, 
                       training.b221$mrs.hudson.score, 
                       training.b221$bastiat.rel.prob) %>% 
    as.matrix()
  
  x_test = data.frame(testing.b221$fl.weighted.average.relevance, 
                      testing.b221$mrs.hudson.score, 
                      testing.b221$bastiat.rel.prob) %>% 
    as.matrix()
  
  y_train = training.b221$true.relevance
  
  y_test = testing.b221$true.relevance
  
  
  #set up the shape of the learning model.
  #spits out errors if you don't have a nice graphics card like me :(
  #but you can ignore these
  
  
  #params for model building
  
  embedding_dims = max_length
  batch_size = nrow(training.b221)/10 # was fixed at = 64
  filters = 32
  kernel_size = 3
  hidden_dims = 100
  
  #too many epochs -> overfitting? this will need validation as we gather more
  #data. in my dissertation I used 1024 which seemed to work given the sparse data
  #but I will be more cautious here.
  
  epochs = 30
  
  ichini.model <- keras_model_sequential() %>% 
    layer_dense(units = 128, activation = 'softmax') %>%
    layer_dropout(0.2) %>%
    layer_dense(units = 10, activation = 'sigmoid') %>%
    layer_dense(1) %>%
    layer_activation("sigmoid") %>% compile(
      loss = "binary_crossentropy",
      optimizer = "nadam",
      metrics = "accuracy"
    )
 
  
  print("Generating new model...")
  
  ichini.model %>% fit(x_train, y_train, epochs = 50, verbose = 2)
  
  print("Model generation complete!")
  
  current.wd = getwd()
  wd.pref = str_extract(getwd(), ".+(?=GTA data team Dropbox)")
  save.wd = paste0(wd.pref, "GTA data team Dropbox/GTA cloud/0 dev/ricardo-freelancer-editor-classifier/ichini_model.h5")
  
  print(paste("Saving new model to", save.wd))
  
  save_model_hdf5(ichini.model, filepath = save.wd)
  
  
  
  #########below is for returning info on model's efficacy##########
  
  
  
  #conf.threshold: e.g. 0.5 means anything below 0.5 will snap to 0, anything above to 1
  # lower threshold if you are worried about type II errors!
  
  cc_make_prediction = function(input.prediction.vect, conf.threshold){
    
    output.prediction.vect = sapply(input.prediction.vect - conf.threshold, ceiling)
    return(output.prediction.vect)
    
  }
  
  
  
  ####TESTING####
  
  testing.b221$test.predictions = model %>% predict_proba(x_test)
  testing.b221$hard.prediction = cc_make_prediction(testing.b221$test.predictions, 0.5)
  testing.b221$correct = testing.b221$true.relevance == testing.b221$hard.prediction
  
  pr.metrics = bt_generate_pr_metrics(model.prediction = testing.b221$hard.prediction, 
                                      real.label = testing.b221$true.relevance, 
                                      model.name = "ichini.model")
  
  print("Finished! Now returning metrics df for your new model...")
  
  return(pr.metrics)
  
  
  
}
