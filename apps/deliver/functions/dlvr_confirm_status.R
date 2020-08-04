dlvr_confirm_status=function(confirm.table = NULL){
  
  # expects a dataframe with columns of hint_id to be changed and the associated validation_classification id's to be confirmed
  
  required.cols = c('hint.id', 'url.classification', 'text.classification', 'jur.classification', 'rlvnt.classification', 'ass.classification', 'int.classification', 'prod.classification', 'date.classification', 'discard.classification')
  if(length(names(confirm.table)[required.cols %in% names(confirm.table)])!=length(required.cols)) return('some expected columns are missing or mislabelled')
  
  gta_sql_get_value(paste0("DROP TABLE IF EXISTS confirm_table;"),db.connection = 'pool')
  gta_sql_create_table(write.df=confirm.table,
                       append.existing = F,
                       table.prefix = '')
  
  confirm.sql.statement = paste0("UPDATE bt_hint_url
                                  JOIN confirm_table ON confirm_table.hint_id = bt_hint_url.hint_id AND confirm_table.url_classification = bt_hint_url.validation_classification
                                  SET confirm_status = 1;
                                  
                                  UPDATE bt_hint_text
                                  JOIN confirm_table ON confirm_table.hint_id = bt_hint_text.hint_id AND confirm_table.url_classification = bt_hint_text.validation_classification
                                  SET confirm_status = 1;
                                  
                                  UPDATE bt_hint_jurisdiction
                                  JOIN confirm_table ON confirm_table.hint_id = bt_hint_jurisdiction.hint_id AND confirm_table.url_classification = bt_hint_jurisdiction.validation_classification
                                  SET confirm_status = 1;
                                  
                                  UPDATE bt_hint_relevance
                                  JOIN confirm_table ON confirm_table.hint_id = bt_hint_relevance.hint_id AND confirm_table.url_classification = bt_hint_relevance.validation_classification
                                  SET confirm_status = 1;
                                  
                                  UPDATE b221_hint_assessment
                                  JOIN confirm_table ON confirm_table.hint_id = b221_hint_assessment.hint_id AND confirm_table.url_classification = b221_hint_assessment.validation_classification
                                  SET confirm_status = 1;
                                  
                                  UPDATE b221_hint_intervention
                                  JOIN confirm_table ON confirm_table.hint_id = b221_hint_intervention.hint_id AND confirm_table.url_classification = b221_hint_intervention.validation_classification
                                  SET confirm_status = 1;
                                  
                                  UPDATE b221_hint_product_group
                                  JOIN confirm_table ON confirm_table.hint_id = b221_hint_product_group.hint_id AND confirm_table.url_classification = b221_hint_product_group.validation_classification
                                  SET confirm_status = 1;
                                  
                                  UPDATE bt_hint_date
                                  JOIN confirm_table ON confirm_table.hint_id = bt_hint_date.hint_id AND confirm_table.url_classification = bt_hint_date.validation_classification
                                  SET confirm_status = 1;
                                  
                                  UPDATE bt_hint_discard_reason
                                  JOIN confirm_table ON confirm_table.hint_id = bt_hint_discard_reason.hint_id AND confirm_table.url_classification = bt_hint_discard_reason.validation_classification
                                  SET confirm_status = 1;")
  gta_sql_multiple_queries(confirm.sql.statement, output.queries = 1, show.time = T, db.connection = 'pool')
  
  gta_sql_get_value(paste0("DROP TABLE IF EXISTS confirm_table;"),db.connection = 'pool')
  
}