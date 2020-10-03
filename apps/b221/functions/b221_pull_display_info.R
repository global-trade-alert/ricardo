b221_pull_display_info = function(is.freelancer = NULL, user.id = NULL){
  
  if(is.null(is.freelancer) | length(is.freelancer)!= 1 | !is.logical(is.freelancer) | is.na(is.freelancer)) stop('is.freelancer must be false if you are an editor, or true if you are a freelancer, no other value permitted')
  
  if(is.freelancer == T){
    pulled.freelancer.hints = gta_sql_get_value("SELECT DISTINCT(bt_hint_processing.hint_id) FROM bt_hint_processing
                                         	  JOIN bt_hint_log ON bt_hint_log.hint_id = bt_hint_processing.hint_id
                                         	  JOIN bt_hint_state_list ON bt_hint_log.hint_state_id = bt_hint_state_list.hint_state_id
                                         	  WHERE bt_hint_processing.user_id = ",user.id," AND (bt_hint_state_list.hint_state_name = 'B221 - freelancer desk')")
    pull.display = b221_freelancer_attribute_picking(hint.vector=pulled.freelancer.hints, only.relevance = F)
  } else {
    pulled.editor.hints = gta_sql_get_value("SELECT DISTINCT(bt_hint_processing.hint_id) FROM bt_hint_processing
                                         	  JOIN bt_hint_log ON bt_hint_log.hint_id = bt_hint_processing.hint_id
                                         	  JOIN bt_hint_state_list ON bt_hint_log.hint_state_id = bt_hint_state_list.hint_state_id
                                         	  WHERE bt_hint_processing.user_id = ",user.id," AND (bt_hint_state_list.hint_state_name = 'B221 - editor desk')")
    pull.display = b221_freelancer_attribute_picking(hint.vector=pulled.editor.hints, only.relevance=F)
  }
  
  # cat(pull.display)
  display=gta_sql_get_value(pull.display)
  return(display)
}
