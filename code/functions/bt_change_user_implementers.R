bt_change_user_implementers=function(app.id = NULL, user.id = NULL, implementer.ids = NULL){
  # this function allows interaction with the table ric_user_implementers
  # the table ric_user_implementer defines a priority list of hints which are prioritised for a given user
  # it is a brute force function, it deletes all implementers with a certain user and replaces them with the new vector
  # if you want to change excuslively china to china + taiwan :) then you should provide both china and taiwan in the new implementer.ids vector
  # note that this function can take multiple app.ids, it only changes the rows for those apps provided!!!
  # only parameter which doesnt accept vector is user.id 
  
  gta_sql_get_value(sprintf(paste0("DELETE FROM ric_user_implementers WHERE user_id = ",user.id," AND app_id IN (%s);"),paste(app.id, collapse = ',')))
  gta_sql_get_value(paste0('INSERT INTO ric_user_implementers VALUES',paste0('(',unique(apply(expand.grid(app.id, user.id, implementer.ids), 1, paste, collapse=",")),')', collapse = ','),';'))
  
  print('successful')
  
}
