bt_delete_hints=function(del.hint.ids = NULL, db.connection = 'pool'){
  
  del.hint.ids = as.numeric(unique(del.hint.ids))
  del.hint.ids = del.hint.ids[!is.na(del.hint.ids)]

  del.sql.statement = sprintf(paste0("DELETE b221_hint_assessment, b221_hint_collection, b221_hint_comment_log, b221_hint_intervention, b221_hint_product_group, bt_classification_log, bt_hint_background_url, bt_hint_bid,
                                  	   bt_hint_evaluation, bt_hint_jurisdiction, bt_hint_lead, bt_hint_processing, bt_hint_relevance, bt_hint_text, bt_hint_url, bt_hint_log 
                                      FROM bt_hint_log
                                      JOIN b221_hint_assessment ON b221_hint_assessment.hint_id = bt_hint_log.hint_id
                                      JOIN b221_hint_collection ON b221_hint_collection.hint_id = bt_hint_log.hint_id
                                      JOIN b221_hint_comment_log ON b221_hint_comment_log.hint_id = bt_hint_log.hint_id
                                      JOIN b221_hint_intervention ON b221_hint_intervention.hint_id = bt_hint_log.hint_id
                                      JOIN b221_hint_product_group ON b221_hint_product_group.hint_id = bt_hint_log.hint_id
                                      JOIN bt_classification_log ON bt_classification_log.hint_id = bt_hint_log.hint_id
                                      JOIN bt_hint_background_url ON bt_hint_background_url.hint_id = bt_hint_log.hint_id
                                      JOIN bt_hint_bid ON bt_hint_bid.hint_id = bt_hint_log.hint_id
                                      JOIN bt_hint_evaluation ON bt_hint_evaluation.hint_id = bt_hint_log.hint_id
                                      JOIN bt_hint_jurisdiction ON bt_hint_jurisdiction.hint_id = bt_hint_log.hint_id
                                      JOIN bt_hint_lead ON bt_hint_lead.hint_id = bt_hint_log.hint_id
                                      JOIN bt_hint_processing ON bt_hint_processing.hint_id = bt_hint_log.hint_id
                                      JOIN bt_hint_relevance ON bt_hint_relevance.hint_id = bt_hint_log.hint_id
                                      JOIN bt_hint_text ON bt_hint_text.hint_id = bt_hint_log.hint_id
                                      JOIN bt_hint_url ON bt_hint_url.hint_id = bt_hint_log.hint_id
                                      WHERE bt_hint_log.hint_id IN (%s);"), paste0(del.hints.ids, collapse = ','))
  
  if(length(del.hint.ids)>0){
    gta_sql_get_value(del.sql.statement, db.connection=db.connection)
    return('successful')
  } else {
    return('no hints valid to delete (only numerically interpretable ids are valid)')
  }
}