b221_freelancer_relevance_decision=function(hint.vector=NULL){
  "
  Function which decides if hints are relevant or not
  Input expects a hint id
  Return outputs 1 if relevant, -1 if irrelevant, 0 if yet undecided (less than 3 freelancer submissions)
  "
  if (is.null(hint.vector)) return('You must enter a vector containing which hints the attributes should be decided for')
  
  trash.or.promote = b221_ichini_classifier(hint.vector, for.training = F)
  print(trash.or.promote)
  
  return(trash.or.promote)
  
}