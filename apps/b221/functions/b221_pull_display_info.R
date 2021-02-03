b221_pull_display_info = function(is.freelancer = NULL, user.id = NULL){
  
  if(is.null(is.freelancer) | length(is.freelancer)!= 1 | !is.logical(is.freelancer) | is.na(is.freelancer)) stop('is.freelancer must be false if you are an editor, or true if you are a freelancer, no other value permitted')
  
  if(is.freelancer == T){
    pulled.hints = gta_sql_get_value(paste("SELECT DISTINCT(bt_hint_processing.hint_id) FROM bt_hint_processing
                                         	  JOIN bt_hint_log ON bt_hint_log.hint_id = bt_hint_processing.hint_id
                                         	  JOIN bt_hint_state_list ON bt_hint_log.hint_state_id = bt_hint_state_list.hint_state_id
                                         	  WHERE bt_hint_processing.user_id = ",user.id," AND (bt_hint_state_list.hint_state_name = 'B221 - freelancer desk')"))
  } else {
    pulled.hints = gta_sql_get_value(paste("SELECT DISTINCT(bt_hint_processing.hint_id) FROM bt_hint_processing
                                         	  JOIN bt_hint_log ON bt_hint_log.hint_id = bt_hint_processing.hint_id
                                         	  JOIN bt_hint_state_list ON bt_hint_log.hint_state_id = bt_hint_state_list.hint_state_id
                                         	  WHERE bt_hint_processing.user_id = ",user.id," AND (bt_hint_state_list.hint_state_name = 'B221 - editor desk')"))
  }
  
  hints.sql=paste("SELECT",pulled.hints[1],"AS hint_id")
  if (length(pulled.hints)>1) hints.sql = paste(hints.sql, paste(pulled.hints[2:length(pulled.hints)], collapse = " UNION SELECT "), sep =" UNION SELECT ")
  
  # pulls those attributes which are null validation
  pull.display = paste0("SELECT DISTINCT GROUP_CONCAT(DISTINCT(jur_list.jurisdiction_name) ORDER BY jur_list.jurisdiction_name ASC SEPARATOR ' ; ') AS jurisdiction_name, ht_log.acting_agency, ht_log.registration_date, ht_log.hint_date, 
                          MAX(CASE WHEN ht_txt.language_id=1 THEN ht_txt.hint_title ELSE NULL END) AS english_title,
                          MAX(CASE WHEN ht_txt.language_id=1 THEN ht_txt.hint_description ELSE NULL END) AS english_description, 
                          MAX(CASE WHEN ht_txt.language_id=2 THEN ht_txt.hint_title ELSE NULL END) AS original_title,
                          MAX(CASE WHEN ht_txt.language_id=2 THEN ht_txt.hint_description ELSE NULL END) AS original_description, bt_url_log.url, attributed_hints.hint_id,
                          GROUP_CONCAT(DISTINCT(cltn_log.collection_id) SEPARATOR ' ; ') AS collection_id,
                          ass_list.assessment_name, 
                          GROUP_CONCAT(DISTINCT(prod_grp_list.product_group_name) SEPARATOR ' ; ') AS product_group_name,
                          GROUP_CONCAT(DISTINCT(int_type_list.intervention_type_name) SEPARATOR ' ; ') AS intervention_type_name,
                          GROUP_CONCAT(DISTINCT(ht_cmt_log.comment) ORDER BY ht_cmt_log.time_stamp DESC SEPARATOR ' ; ') AS comment,
                          bhr.relevance, bhu.url_type_id,
                          MAX(IF(bt_date_type_list.date_type_name='announcement', bt_hint_date.date, NULL )) AS announcement_date,
                          MAX(IF(bt_date_type_list.date_type_name='implementation', bt_hint_date.date, NULL )) AS implementation_date,
                          MAX(IF(bt_date_type_list.date_type_name='removal', bt_hint_date.date, NULL )) AS removal_date,
                          MAX(IF(bt_discard_reason_list.discard_reason_name = 'other (see comment)',bt_hint_discard_reason.discard_reason_comment,bt_discard_reason_list.discard_reason_name)) AS discard_reason
                          FROM (",hints.sql,") attributed_hints
                          JOIN bt_hint_log ht_log ON ht_log.hint_id = attributed_hints.hint_id
                          JOIN bt_hint_state_list ht_state ON ht_log.hint_state_id = ht_state.hint_state_id AND (ht_state.hint_state_name = 'B221 - freelancer desk' OR ht_state.hint_state_name = 'B221 - editor desk' OR ht_state.hint_state_name = 'trash bin - entered')
                          LEFT JOIN bt_hint_relevance bhr ON bhr.hint_id = attributed_hints.hint_id AND (bhr.relevance_accepted = 1 OR bhr.relevance_accepted IS NULL) AND bhr.validation_classification IS NULL LEFT JOIN bt_classification_log class_rlvc ON class_rlvc.classification_id = bhr.classification_id 
                          LEFT JOIN bt_hint_url bhu ON bhu.hint_id = attributed_hints.hint_id AND (bhu.url_accepted = 1 OR bhu.url_accepted IS NULL) AND bhu.classification_id = bhr.classification_id 
                          LEFT JOIN bt_url_log ON bt_url_log.url_id = bhu.url_id
                          LEFT JOIN bt_url_type_list ON bhu.url_type_id = bt_url_type_list.url_type_id
                          LEFT JOIN bt_hint_text ht_txt ON ht_txt.hint_id = attributed_hints.hint_id AND (ht_txt.description_accepted = 1 OR ht_txt.description_accepted IS NULL)
                          LEFT JOIN bt_hint_jurisdiction ht_jur ON ht_jur.hint_id = attributed_hints.hint_id AND (ht_jur.jurisdiction_accepted = 1 OR ht_jur.jurisdiction_accepted IS NULL) AND ht_jur.classification_id = bhr.classification_id LEFT JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_id = ht_jur.jurisdiction_id
                          LEFT JOIN b221_hint_collection ht_cltn ON ht_cltn.hint_id = attributed_hints.hint_id LEFT JOIN b221_collection_log cltn_log ON cltn_log.collection_id = ht_cltn.collection_id
                          LEFT JOIN b221_hint_assessment ht_ass ON ht_ass.hint_id = attributed_hints.hint_id AND (ht_ass.assessment_accepted = 1 OR ht_ass.assessment_accepted IS NULL) AND ht_ass.classification_id = bhr.classification_id LEFT JOIN b221_assessment_list ass_list ON ass_list.assessment_id = ht_ass.assessment_id
                          LEFT JOIN b221_hint_product_group prod_grp ON prod_grp.hint_id = attributed_hints.hint_id AND (prod_grp.product_group_assessment = 1 OR prod_grp.product_group_assessment IS NULL) AND prod_grp.classification_id = bhr.classification_id LEFT JOIN b221_product_group_list prod_grp_list ON prod_grp_list.product_group_id = prod_grp.product_group_id
                          LEFT JOIN b221_hint_intervention ht_int ON ht_int.hint_id = attributed_hints.hint_id AND (ht_int.intervention_accepted = 1 OR ht_int.intervention_accepted IS NULL) AND ht_int.classification_id = bhr.classification_id LEFT JOIN b221_intervention_type_list int_type_list ON ht_int.apparent_intervention_id = int_type_list.intervention_type_id
                          LEFT JOIN bt_hint_date ON bt_hint_date.hint_id = attributed_hints.hint_id AND (bt_hint_date.date_accepted = 1 OR bt_hint_date.date_accepted IS NULL) AND bt_hint_date.classification_id = bhr.classification_id LEFT JOIN bt_date_type_list ON bt_hint_date.date_type_id = bt_date_type_list.date_type_id
                          LEFT JOIN bt_hint_discard_reason ON bt_hint_discard_reason.hint_id = attributed_hints.hint_id AND (bt_hint_date.date_accepted = 1 OR bt_hint_discard_reason.reason_accepted IS NULL) AND bt_hint_discard_reason.classification_id = bhr.classification_id LEFT JOIN bt_discard_reason_list ON bt_hint_discard_reason.discard_reason_id = bt_discard_reason_list.discard_reason_id
                          LEFT JOIN b221_hint_comment_log ht_cmt_log ON ht_cmt_log.hint_id = attributed_hints.hint_id
                          GROUP BY attributed_hints.hint_id;")
  
  
  # cat(pull.display)
  display=gta_sql_get_value(pull.display)
  return(display)
}
