bt_attribute_hint_processing = function(user.id = NULL, hint.state = NULL, db.connection = 'pool'){
  
  states = gta_sql_get_value('SELECT hint_state_name FROM `bt_hint_state_list`;')
  
  if(!hint.state %in% states[grep('B221|OSC',states)] | length(hint.state)!=1) stop('can only attribute hints to users of a single bastiat state (app stage), also include the string name of the state, not the id')
  
  #hint.state == 'B221 - editor desk'
  if(3<2) hint.state = paste0("'",hint.state, "' OR bt_hint_state_list.hint_state_name = 'trash bin - entered'") else hint.state = paste0("'",hint.state,"'")
  app.id = gta_sql_get_value(paste0("SELECT type_id FROM ric_app_list WHERE app_name = ",substr(hint.state,1,5),"';"))
  sql.pull.hints = paste0("SELECT DISTINCT hint_id FROM
                          (SELECT hint_id FROM
                          (SELECT bt_hint_log.hint_id FROM bt_hint_log 
                          JOIN bt_hint_jurisdiction ht_jur ON ht_jur.hint_id = bt_hint_log.hint_id 
                          JOIN (SELECT jurisdiction_id FROM ric_user_implementers WHERE user_id = ",user.id," AND app_id = ",app.id,") user_prio_jur ON ht_jur.jurisdiction_id = user_prio_jur.jurisdiction_id
                          LEFT JOIN b221_hint_intervention ON b221_hint_intervention.hint_id = bt_hint_log.hint_id
                          JOIN bt_hint_text ON bt_hint_text.hint_id = bt_hint_log.hint_id AND bt_hint_text.hint_description IS NOT NULL AND bt_hint_text.hint_description != '' AND bt_hint_text.hint_title IS NOT NULL AND bt_hint_text.hint_title != ''
                          JOIN bt_hint_state_list ON bt_hint_log.hint_state_id = bt_hint_state_list.hint_state_id AND (bt_hint_state_list.hint_state_name = ",hint.state,") 
                          AND NOT EXISTS (SELECT NULL FROM bt_hint_processing WHERE bt_hint_log.hint_id = bt_hint_processing.hint_id) 
                          ORDER BY FIND_IN_SET(apparent_intervention_id, '2,3') DESC, bt_hint_log.registration_date DESC LIMIT 10) prio_hints
                          UNION 
                          SELECT hint_id FROM
                          (SELECT bt_hint_log.hint_id FROM bt_hint_log 
                          LEFT JOIN b221_hint_intervention ON b221_hint_intervention.hint_id = bt_hint_log.hint_id
                          JOIN bt_hint_text ON bt_hint_text.hint_id = bt_hint_log.hint_id AND bt_hint_text.hint_description IS NOT NULL AND bt_hint_text.hint_description != '' AND bt_hint_text.hint_title IS NOT NULL AND bt_hint_text.hint_title != ''
                          JOIN bt_hint_state_list ON bt_hint_log.hint_state_id = bt_hint_state_list.hint_state_id AND (bt_hint_state_list.hint_state_name = ",hint.state,") 
                          AND NOT EXISTS (SELECT NULL FROM bt_hint_processing WHERE bt_hint_log.hint_id = bt_hint_processing.hint_id)
                          ORDER BY FIND_IN_SET(apparent_intervention_id, '2,3') DESC, bt_hint_log.registration_date DESC LIMIT 10) non_prio_hints) hints LIMIT 10;")
  
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