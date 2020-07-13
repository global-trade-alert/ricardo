b221_hint_url=function(is.freelancer = NULL, user.id = NULL, hint.url.dataframe = NULL){
  # hint.url.dataframe = data.frame(hint.id = 1:3, url = c("https://www.wto.org/english/tratop_e/covid19_e/covid_measures_e.pdf","http://www.douane.gov.dz/s","http://servicios.infoleg.gob.a"), is.official = c(1,0,1))
  # is.freelancer = T
  # user.id = 1
  
  temp.changes.name=paste0("b221.url.changes.",user.id)
  assign(temp.changes.name,hint.url.dataframe,envir=globalenv())
  
  gta_sql_get_value(paste0("DROP TABLE IF EXISTS ",gsub('\\.','_',temp.changes.name),";"),db.connection = 'pool')
  gta_sql_create_table(write.df=temp.changes.name,
                       append.existing = F,
                       table.prefix = '')
  
  if(is.freelancer==T){
    push.updates = paste0("SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log' AND TABLE_SCHEMA = 'ricardomainclone');
                          INSERT INTO bt_classification_log(classification_id, user_id, hint_state_id, time_stamp)
                          SELECT DISTINCT @classification_id AS classification_id, ",user.id," AS user_id, (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - freelancer desk') AS hint_state_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp; 
    
                          INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_user)
                          SELECT changes_w_url_type.hint_id, bt_url_log.url_id, changes_w_url_type.url_type_id, @classification_id AS classification_id, NULL AS url_accepted, NULL AS validation_user FROM 
                          (SELECT DISTINCT changes.hint_id, changes.url, (CASE WHEN changes.is_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id
                          FROM b221_url_changes_",user.id," changes) changes_w_url_type
                          JOIN bt_url_log ON changes_w_url_type.url = bt_url_log.url
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_url ht_url WHERE ht_url.hint_id = changes_w_url_type.hint_id AND ht_url.url_id = bt_url_log.url_id AND ht_url.url_type_id = changes_w_url_type.url_type_id);
                          
                          UPDATE bt_hint_url ht_url 
                          JOIN (SELECT DISTINCT hint_id FROM b221_url_changes_",user.id,") changed_hints ON ht_url.hint_id = changed_hints.hint_id
                          JOIN bt_url_log ON bt_url_log.url_id = ht_url.url_id
                          LEFT JOIN (SELECT DISTINCT changes.hint_id, changes.url, (CASE WHEN changes.is_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id
                          FROM b221_url_changes_",user.id," changes) changes_w_url_type
                          ON ht_url.hint_id = changes_w_url_type.hint_id AND changes_w_url_type.url = bt_url_log.url AND changes_w_url_type.url_type_id = ht_url.url_type_id
                          SET ht_url.url_accepted = (CASE WHEN changes_w_url_type.hint_id IS NOT NULL THEN NULL ELSE 0 END),
                          ht_url.classification_id = @classification_id;")
  } else {
    push.updates = paste0("SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log' AND TABLE_SCHEMA = 'ricardomainclone');
                          INSERT INTO bt_classification_log (classification_id, user_id, hint_state_id, time_stamp)
                          SELECT DISTINCT @classification_id AS classification_id, ",user.id," AS user_id, (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - editor desk') AS hint_state_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp;
                          
                          INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_user)
                          SELECT changes_w_url_type.hint_id, bt_url_log.url_id, changes_w_url_type.url_type_id, @classification_id AS classification_id, 1 AS url_accepted, ",user.id," AS validation_user FROM 
                          (SELECT DISTINCT changes.hint_id, changes.url, (CASE WHEN changes.is_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id
                          FROM b221_url_changes_",user.id," changes) changes_w_url_type
                          JOIN bt_url_log ON changes_w_url_type.url = bt_url_log.url
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_url ht_url WHERE ht_url.hint_id = changes_w_url_type.hint_id AND ht_url.url_id = bt_url_log.url_id AND ht_url.url_type_id = changes_w_url_type.url_type_id);
                          
                          UPDATE bt_hint_url ht_url 
                          JOIN (SELECT DISTINCT hint_id FROM b221_url_changes_",user.id,") changed_hints ON ht_url.hint_id = changed_hints.hint_id
                          JOIN bt_url_log ON bt_url_log.url_id = ht_url.url_id
                          LEFT JOIN (SELECT DISTINCT changes.hint_id, changes.url, (CASE WHEN changes.is_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id
                          	   FROM b221_url_changes_",user.id," changes) changes_w_url_type
                          ON ht_url.hint_id = changes_w_url_type.hint_id AND changes_w_url_type.url = bt_url_log.url AND changes_w_url_type.url_type_id = ht_url.url_type_id
                          SET ht_url.validation_user = ",user.id,", 
                          ht_url.url_accepted = (CASE WHEN changes_w_url_type.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          UPDATE bt_hint_log
                          JOIN 
                          (SELECT b221_hint_collection.hint_id, changed_collection.max_state AS new_state FROM b221_hint_collection
                          JOIN
                          (SELECT DISTINCT locate_collection_hints.collection_id,
                          MAX(IF(hint_state_id BETWEEN 3 AND 4 AND is_official = 1, 5, hint_state_id)) max_state
                          FROM b221_url_changes_",user.id," changes
                          JOIN b221_hint_collection allocate_collection ON changes.hint_id = allocate_collection.hint_id
                          JOIN b221_hint_collection locate_collection_hints ON allocate_collection.collection_id = locate_collection_hints.collection_id
                          JOIN bt_hint_log ON locate_collection_hints.hint_id = bt_hint_log.hint_id
                          GROUP BY locate_collection_hints.collection_id) changed_collection ON b221_hint_collection.collection_id = changed_collection.collection_id) new_hint_states ON bt_hint_log.hint_id = new_hint_states.hint_id
                          SET bt_hint_log.hint_state_id = new_hint_states.new_state;")  
  }
  gta_sql_multiple_queries(push.updates, db.connection = 'pool', output.queries = 1)
  gta_sql_get_value(paste0("DROP TABLE IF EXISTS ",gsub('\\.','_',temp.changes.name),";"),db.connection = 'pool')
  return('successful')
  
}