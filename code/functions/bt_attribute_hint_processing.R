bt_attribute_hint_processing = function(user.id = NULL, hint.state = NULL, db.connection = 'pool'){
  
  states = gta_sql_get_value('SELECT hint_state_name FROM `bt_hint_state_list`;')
  
  if(!hint.state %in% states[grep('B221|OSC',states)] | length(hint.state)!=1) stop('can only attribute hints to users of a single bastiat state (app stage), also include the string name of the state, not the id')
  
  #hint.state == 'B221 - editor desk'
  if(3<2) hint.state = paste0("'",hint.state, "' OR bt_hint_state_list.hint_state_name = 'trash bin - entered'") else hint.state = paste0("'",hint.state,"'")
  sql.pull.hints = paste0("SELECT DISTINCT(bt_hint_log.hint_id) FROM bt_hint_log ",
                          "LEFT JOIN bt_hint_jurisdiction ht_jur ON ht_jur.hint_id = bt_hint_log.hint_id ",
                          "LEFT JOIN ric_app_settings ON ht_jur.jurisdiction_id = ric_app_settings.parameter_value AND ric_app_settings.app_id = 4 AND ric_app_settings.parameter_name = 'implementer' ",
                          "JOIN bt_hint_state_list ON bt_hint_log.hint_state_id = bt_hint_state_list.hint_state_id AND (bt_hint_state_list.hint_state_name = ",hint.state,") ",
                          "AND NOT EXISTS (SELECT NULL FROM bt_hint_processing WHERE bt_hint_log.hint_id = bt_hint_processing.hint_id) ",
                          "ORDER BY hint_id LIMIT 50;")
  
  hints <<- na.omit(data.frame(hint.id = gta_sql_get_value(sql.pull.hints), user.id = user.id, start.time = substr(as.POSIXct(Sys.time(), tz = "CET"),1,19)))

  if(nrow(hints)<1){
    processing.hints = 'no hints at this stage to process sadly!'
  } else {
    gta_sql_append_table(table.prefix = 'bt_', append.table = 'hint.processing', append.by.df = 'hints', db.connection = db.connection)
    processing.hints = hints$hint.id
  }
  
  return(processing.hints) 
  # emergency delete upon interruption would be 
  # DELETE bt_hint_processing FROM bt_hint_processing
  # JOIN bt_hint_log ON bt_hint_log.hint_id = bt_hint_processing.hint_id
  # JOIN bt_hint_state_list ON bt_hint_log.hint_state_id = bt_hint_state_list.hint_state_id
  # WHERE bt_hint_processing.user_id = user.id AND bt_hint_state_list.hint_state_name = 'OSC - freelancer or editor desk';
  # this is able to handle freelancers/editors using both b221 + osc at the same time
}


# library(gtasql)
# library(gtalibrary)
# library(pool)
# gta_setwd()
# gta_sql_pool_open(db.title="ricardomain",
#                   db.host = gta_pwd("ricardomain")[['host']],
#                   db.name = gta_pwd("ricardomain")[['name']],
#                   db.user = gta_pwd("ricardomain")[['user']],
#                   db.password = gta_pwd("ricardomain")[['password']],
#                   table.prefix = "ric_")
# 
# user.id = 40
# hint.state = 'B221 - freelancer desk'
# source('17 Shiny/8 ricardo app/apps/b221/functions/b221_process_collections.R')
# b221_process_collections_hints(is.freelancer = T, user.id = 40, new.collection.name = NULL, collection.id = NULL, hints.id = NULL, country = NULL,
#                                         product = NULL, intervention = NULL, assessment = NULL, relevance = NULL)