b221_freelancer_attribute_picking=function(hint.vector=NULL, only.relevance = F){
  "
  Function which decides which attributes are valid for a hint vector
  If only.relevance = T then only evaluates whether the hint should be promoted or not - this is useful for determining in the b221_process_display if hints should be set to stage 3 (-1 means trash, 1 promotion, 0 do nothing)
  if stage 3 or higher then it will not promote regardless of relevance
  If only.relevance = F then returns entire set of attributes, please follow the b221_pull_display function's column order/format as this is directly fed there!
  
  "
  if (is.null(input.dataframe)) return('You must enter a vector containing which hints the attributes should be decided for')
  
  hints.sql=paste("SELECT",hint.vector[1],"AS hint_id")
  if (length(hint.vector)>1) paste(hints.sql, paste(hint.vector[2:length(hint.vector)], collapse = " UNION SELECT "), sep =" UNION SELECT ")
  
  # this is used line 234 of the b221_process_display_info to know whether to promote or bin certain hints
  # algorithm to determine promotion - output is "hint.id, promote" dataframe where promote is 1 or -1, 1 is promote, -1 is trash, do nothing is not output
  if(only.relevance){
    
    # placeholder query for now, prefer not to write a complex algorithm and implement it only for us to disagree on the details of it
    sql.relevance = paste0("SELECT changed_hints.hint_id, IF(SUM(bhr.relevance)>=2,1,-1) AS promote
                            FROM (",hints.sql,") changed_hints
                            JOIN bt_hint_relevance bhr ON bhr.hint_id = changed_hints.hint_id AND bhr.validation_classification IS NULL
                            JOIN bt_classification_log bcl ON bcl.classification_id = bhr.classification_id
                            JOIN bt_hint_log bhl ON bhl.hint_id = changed_hints.hint_id AND bhl.hint_state_id < 3
                            GROUP BY bhr.hint_id HAVING COUNT(bhr.hint_id)=3;")
    
    trash.or.promote = gta_sql_get_value(sql.relevance)
    return(trash.or.promote)
    
  } else {
    
    # algorithm input can be either the hints in state 1 or state 2
    # algorithm determines attributes both that the next freelancer in the chain would see, or the next editor ( i assume we would use the same algorithm )
    algorithm.input = paste0("SELECT DISTINCT attributed_hints.hint_id, ht_log.hint_state_id, ht_log.acting_agency, ht_log.registration_date, ht_log.hint_date,
                              jur_list.jurisdiction_name AS jurisdiction_name, bt_url_log.url, bhu.url_type_id, cltn_log.collection_id, ass_list.assessment_name, prod_grp_list.product_group_name, int_type_list.intervention_type_name, bhr.relevance, 
                              MAX(CASE WHEN ht_txt.language_id=1 THEN ht_txt.hint_title ELSE NULL END) AS english_title,
                              MAX(CASE WHEN ht_txt.language_id=1 THEN ht_txt.hint_description ELSE NULL END) AS english_description,
                              MAX(CASE WHEN ht_txt.language_id=2 THEN ht_txt.hint_title ELSE NULL END) AS original_title,
                              MAX(CASE WHEN ht_txt.language_id=2 THEN ht_txt.hint_description ELSE NULL END) AS original_description, bt_url_log.url, attributed_hints.hint_id,
                              IF(bt_date_type_list.date_type_name='announcement', bt_hint_date.date, NULL ) AS announcement_date,
                              IF(bt_date_type_list.date_type_name='implementation', bt_hint_date.date, NULL ) AS implementation_date,
                              IF(bt_date_type_list.date_type_name='removal', bt_hint_date.date, NULL ) AS removal_date,
                              IF(bt_discard_reason_list.discard_reason_name = 'other (see comment)',bt_hint_discard_reason.discard_reason_comment,bt_discard_reason_list.discard_reason_name) AS discard_reason,
                              GROUP_CONCAT(DISTINCT(ht_cmt_log.comment) ORDER BY ht_cmt_log.time_stamp DESC SEPARATOR ' ; ') AS comment
                              FROM (",hint.sql,") attributed_hints
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
                              GROUP BY attributed_hints.hint_id, class_rlvc.user_id, bhu.url_type_id, jur_list.jurisdiction_name, prod_grp_list.product_group_name, int_type_list.intervention_type_name, ass_list.assessment_id , bt_hint_date.`date`, bt_hint_date.date_type_id, bhr.relevance, bt_hint_discard_reason.discard_reason_id;")
    
    # Your algorithm here
    # The following columns should be output after the algorithm as ' ; ' separated and grouped by hint.id (one row per hint): jurisdiction.name, product.group.name, intervention.type.name
    # underneath is the list of columns expected by the ui and their ordering
    
    
    
    
    output.column.order = c("jurisdiction.name","acting.agency","registration.date","hint.date","english.title","english.description","original.title","original.description","url","hint.id","collection.id",
                            "assessment.name","product.group.name","intervention.type.name","comment","relevance","url.type.id","announcement.date","implementation.date","removal.date","discard.reason")
    
  }
  
  
  
}