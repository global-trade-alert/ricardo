dlvr_pull_display=function(){
  sql.statement = paste0("SELECT IF(bt_hint_log.gta_id IS NOT NULL,bt_hint_log.gta_id,b221_collection_star.hint_id) entry_id, 1 AS entry_type, gta_jurisdiction_list.jurisdiction_name AS country, b221_assessment_list.assessment_name AS initial_assessment, 
                          IF(bt_hint_log.gta_id IS NOT NULL,b221_intervention_type_list.intervention_type_name,NULL) AS gta_intervention_type, dates.date_announced , dates.date_implemented, dates.date_removed,
                          bt_hint_text.hint_description AS description, src.url
                          FROM b221_collection_star
                          JOIN bt_hint_log ON b221_collection_star.hint_id = bt_hint_log.hint_id AND bt_hint_log.hint_state_id BETWEEN 3 AND 7
                          LEFT JOIN bt_hint_jurisdiction ON bt_hint_jurisdiction.hint_id = b221_collection_star.hint_id AND bt_hint_jurisdiction.jurisdiction_accepted = 1 LEFT JOIN gta_jurisdiction_list ON gta_jurisdiction_list.jurisdiction_id = bt_hint_jurisdiction.jurisdiction_id
                          LEFT JOIN b221_hint_assessment ON b221_hint_assessment.hint_id = b221_collection_star.hint_id AND b221_hint_assessment.assessment_accepted = 1 LEFT JOIN b221_assessment_list ON b221_assessment_list.assessment_id = b221_hint_assessment.assessment_id
                          LEFT JOIN b221_hint_intervention ON b221_hint_intervention.hint_id = b221_collection_star.hint_id AND bt_hint_jurisdiction.jurisdiction_accepted = 1 LEFT JOIN b221_intervention_type_list ON b221_intervention_type_list.intervention_type_id = b221_hint_intervention.apparent_intervention_id
                          LEFT JOIN (SELECT bt_hint_date.hint_id,
                          			MAX(IF(bt_date_type_list.date_type_name='announcement', bt_hint_date.date, NULL )) AS date_announced,
                          			MAX(IF(bt_date_type_list.date_type_name='implementation', bt_hint_date.date, NULL )) AS date_implemented,
                          			MAX(IF(bt_date_type_list.date_type_name='removal', bt_hint_date.date, NULL )) AS date_removed 
                          			FROM bt_hint_date, bt_date_type_list WHERE bt_hint_date.date_type_id = bt_date_type_list.date_type_id AND bt_hint_date.date_accepted = 1 GROUP BY hint_id) dates ON dates.hint_id = b221_collection_star.hint_id
                          LEFT JOIN bt_hint_text ON bt_hint_text.hint_id = b221_collection_star.hint_id AND bt_hint_text.description_accepted = 1
                          LEFT JOIN (SELECT bt_hint_url.hint_id, GROUP_CONCAT(bt_url_log.url SEPARATOR ' ; ') AS url FROM bt_hint_url, bt_url_log 
                          			WHERE bt_hint_url.url_accepted = 1 AND bt_url_log.url_id = bt_hint_url.url_id
                          			GROUP BY bt_hint_url.hint_id) src ON src.hint_id = b221_collection_star.hint_id;
                          ")
  display = gta_sql_get_value(sql.statement)
  
  ## to be finished
  
  return(display)
}