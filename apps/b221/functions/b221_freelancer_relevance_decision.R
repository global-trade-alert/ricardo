b221_freelancer_relevance_decision=function(hint.vector=NULL, model.folder="17 Shiny/8 ricardo app/apps/b221/promotion-demotion-classifier/"){
  "
  Function which decides if hints are relevant or not
  Input expects a hint id
  Return outputs 1 if relevant, -1 if irrelevant, 0 if yet undecided (less than 3 freelancer submissions)
  "
  if (is.null(hint.vector)) return('You must enter a vector containing which hints the attributes should be decided for')
  
  trash.or.promote = b221_ichini_classifier(hint.vector, for.training = F, model.folder=model.folder)
  print(trash.or.promote)
  
  return(trash.or.promote)
  
}