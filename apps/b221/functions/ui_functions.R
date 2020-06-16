generate_initial_hints <- function(df = initSingleHint, initSingle = F) {
  
  added <- ifelse(initSingle, " ", " added")
  official <- ifelse(is.na(df$official), ""," official")
  df$checkbox <- '<img class="official" src="www/b221/official.svg"><img class="nonofficial" src="www/b221/nonofficial.svg">'
  df$middle <- ifelse(df$is.intervention == 0, paste0('<a class="url" href="',df$url,'" target="_blank"><img src="www/b221/urlwhite.svg"></a>'),paste0('<a class="url" href="https://www.globaltradealert.org/state-act/',df$state.act.id,'" target="_blank"><img src="www/b221/urlwhite.svg"></a><div class="middle"><img class="intervention-icon" src="www/b221/intervention.svg"></div>'))
  
  df$processed <- ifelse(df$hint.state.id>2, '<img class="processed" src="www/b221/processed.svg">', '<img class="unprocessed" src="www/b221/unprocessed.svg">')
  output <- paste0('<div data-tooltip-content="#top-tooltip_',df$hint.id,'" id="hintId_',df$hint.id,'" class="hint-item tooltip-create-top',added,official,'"><div class="top"><div class="hint-title">',df$hint.title,'</div><div class="remove" value="',df$hint.id,'"><img src="www/b221/cancel.svg"></div><span class="material-icons">stars</span></div><div class="bottom-options"><div class="is-official">',df$checkbox,'</div>',df$middle,'<div class="is-processed">',df$processed,'</div></div></div>',df$tpcontent)
  return(output)
}

get_single_hints_infos <- function(user.id = NULL) {
  output = gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT * FROM
                                                          (SELECT ht_log.hint_id, ht_log.hint_state_id, ht_log.acting_agency, ht_log.registration_date AS hint_date, jur_list.jurisdiction_name, CONCAT(IF(bt_hint_state_act.state_act_id IS NULL,'',CONCAT('State act ',bt_hint_state_act.state_act_id,' - Intervention ',ht_log.gta_id,' - ')),ht_txt.hint_title) AS hint_title, ht_txt.hint_description, ass_list.assessment_name, user_prio_impl.jurisdiction_id AS prio_cty, cltn_log.collection_id, cltn_log.collection_name,
                                                          GROUP_CONCAT(DISTINCT int_list.intervention_type_name SEPARATOR ' ; ')  AS intervention_type, 
                                                          GROUP_CONCAT(DISTINCT prod_list.product_group_name SEPARATOR ' ; ')  AS product_group_name,
                                                          GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='official', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS official,
                                                          GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='news', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS news,
                                                          GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='consultancy', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS consultancy,
                                                          GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='others', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS others,
                                                          (CASE WHEN ht_log.gta_id IS NOT NULL THEN 1 ELSE 0 END) AS is_intervention
                                                          FROM bt_hint_log ht_log 
                                                          JOIN bt_hint_url ht_url ON ht_url.hint_id = ht_log.hint_id AND ht_log.hint_state_id BETWEEN 2 and 9 JOIN bt_url_log ON ht_url.url_id = bt_url_log.url_id JOIN bt_url_type_list ON bt_url_type_list.url_type_id = ht_url.url_type_id
                                                          LEFT JOIN bt_hint_jurisdiction ht_jur ON ht_log.hint_id = ht_jur.hint_id LEFT JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_id = ht_jur.jurisdiction_id
                                                          LEFT JOIN (SELECT jurisdiction_id FROM ric_user_implementers WHERE user_id = ",user.id," AND app_id = 4) user_prio_impl ON user_prio_impl.jurisdiction_id = ht_jur.jurisdiction_id
                                                          LEFT JOIN bt_hint_text ht_txt ON ht_txt.hint_id = ht_log.hint_id AND language_id = 1
                                                          LEFT JOIN b221_hint_assessment ht_ass ON ht_ass.hint_id = ht_log.hint_id LEFT JOIN b221_assessment_list ass_list ON ass_list.assessment_id = ht_ass.assessment_id
                                                          LEFT JOIN b221_hint_intervention ht_int ON ht_int.hint_id = ht_log.hint_id LEFT JOIN b221_intervention_type_list int_list ON int_list.intervention_type_id = ht_int.apparent_intervention_id
                                                          LEFT JOIN b221_hint_product_group ht_prod_grp ON ht_prod_grp.hint_id = ht_log.hint_id LEFT JOIN b221_product_group_list prod_list ON prod_list.product_group_id = ht_prod_grp.product_group_id
                                                          LEFT JOIN bt_hint_state_act ON bt_hint_state_act.hint_id = ht_log.hint_id 
                                                          LEFT JOIN b221_hint_collection ht_cltn ON ht_cltn.hint_id = ht_log.hint_id LEFT JOIN b221_collection_log cltn_log ON cltn_log.collection_id = ht_cltn.collection_id
                                                          GROUP BY ht_log.hint_id) unsorted_hints
                                                          ORDER BY prio_cty DESC, hint_date DESC;")))
  
  return(output)
}

get_info_by_hint_id <- function(hint.id = NULL) {
  output <- gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT * FROM
                                                            (SELECT ht_log.hint_id, ht_log.hint_state_id, ht_log.acting_agency, ht_log.hint_date, jur_list.jurisdiction_name, CONCAT(IF(ht_act.state_act_id IS NULL,'',CONCAT('State act ',ht_act.state_act_id,' - Intervention ',ht_log.gta_id,' - ')),ht_txt.hint_title) AS hint_title, ht_txt.hint_description, ass_list.assessment_name, cltn_log.collection_id, cltn_log.collection_name,
                                                            GROUP_CONCAT(DISTINCT int_list.intervention_type_name SEPARATOR ' ; ')  AS intervention_type, 
                                                            GROUP_CONCAT(DISTINCT prod_list.product_group_name SEPARATOR ' ; ')  AS product_group_name,
                                                            GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='official', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS official,
                                                            GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='news', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS news,
                                                            GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='consultancy', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS consultancy,
                                                            GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='others', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS others,
                                                            (CASE WHEN ht_log.gta_id IS NOT NULL THEN 1 ELSE 0 END) AS is_intervention,
                                                            ht_act.state_act_id
                                                            FROM bt_hint_log ht_log 
                                                            JOIN bt_hint_url ht_url ON ht_url.hint_id = ht_log.hint_id JOIN bt_url_log ON ht_url.url_id = bt_url_log.url_id JOIN bt_url_type_list ON bt_url_type_list.url_type_id = ht_url.url_type_id
                                                            LEFT JOIN bt_hint_jurisdiction ht_jur ON ht_log.hint_id = ht_jur.hint_id LEFT JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_id = ht_jur.jurisdiction_id
                                                            LEFT JOIN bt_hint_text ht_txt ON ht_txt.hint_id = ht_log.hint_id AND language_id = 1
                                                            LEFT JOIN b221_hint_assessment ht_ass ON ht_ass.hint_id = ht_log.hint_id LEFT JOIN b221_assessment_list ass_list ON ass_list.assessment_id = ht_ass.assessment_id
                                                            LEFT JOIN b221_hint_intervention ht_int ON ht_int.hint_id = ht_log.hint_id LEFT JOIN b221_intervention_type_list int_list ON int_list.intervention_type_id = ht_int.apparent_intervention_id
                                                            LEFT JOIN b221_hint_product_group ht_prod_grp ON ht_prod_grp.hint_id = ht_log.hint_id LEFT JOIN b221_product_group_list prod_list ON prod_list.product_group_id = ht_prod_grp.product_group_id
                                                            LEFT JOIN b221_hint_collection ht_cltn ON ht_cltn.hint_id = ht_log.hint_id LEFT JOIN b221_collection_log cltn_log ON cltn_log.collection_id = ht_cltn.collection_id
                                                            LEFT JOIN bt_hint_state_act as ht_act ON ht_act.hint_id = ht_log.hint_id 
                                                            WHERE ht_log.hint_id = ",hint.id,"
                                                            GROUP BY ht_log.hint_id) unsorted_hints;")))
  
  return(output)
}

get_info_by_collection_id <- function(collection.id = NULL) {
  output <- unique(gta_sql_get_value(paste0("SELECT * FROM
                                              (SELECT ht_log.hint_id, ht_log.hint_state_id, ht_log.acting_agency, ht_log.hint_date, jur_list.jurisdiction_name, ht_txt.hint_title, ht_txt.hint_description, ass_list.assessment_name, cltn_log.collection_id, cltn_log.collection_name,
                                              GROUP_CONCAT(DISTINCT int_list.intervention_type_name SEPARATOR ' ; ')  AS intervention_type, 
                                              GROUP_CONCAT(DISTINCT prod_list.product_group_name SEPARATOR ' ; ')  AS product_group_name,
                                              GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='official', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS official,
                                              GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='news', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS news,
                                              GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='consultancy', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS consultancy,
                                              GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='others', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS others,
                                              (CASE WHEN ht_log.gta_id IS NOT NULL THEN 1 ELSE 0 END) AS is_intervention
                                              FROM bt_hint_log ht_log 
                                              JOIN b221_hint_collection ht_col ON ht_col.hint_id = ht_log.hint_id AND ht_col.collection_id = ",collection.id,"
                                              JOIN bt_hint_url ht_url ON ht_url.hint_id = ht_log.hint_id JOIN bt_url_log ON ht_url.url_id = bt_url_log.url_id AND (ht_url.url_accepted = 1 OR ht_url.url_accepted IS NULL) AND ht_url.classification_id IS NOT NULL
                                              JOIN bt_url_type_list ON bt_url_type_list.url_type_id = ht_url.url_type_id
                                              LEFT JOIN bt_hint_jurisdiction ht_jur ON ht_log.hint_id = ht_jur.hint_id LEFT JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_id = ht_jur.jurisdiction_id
                                              LEFT JOIN bt_hint_text ht_txt ON ht_txt.hint_id = ht_log.hint_id AND language_id = 1
                                              LEFT JOIN b221_hint_assessment ht_ass ON ht_ass.hint_id = ht_log.hint_id LEFT JOIN b221_assessment_list ass_list ON ass_list.assessment_id = ht_ass.assessment_id
                                              LEFT JOIN b221_hint_intervention ht_int ON ht_int.hint_id = ht_log.hint_id LEFT JOIN b221_intervention_type_list int_list ON int_list.intervention_type_id = ht_int.apparent_intervention_id
                                              LEFT JOIN b221_hint_product_group ht_prod_grp ON ht_prod_grp.hint_id = ht_log.hint_id LEFT JOIN b221_product_group_list prod_list ON prod_list.product_group_id = ht_prod_grp.product_group_id
                                              LEFT JOIN b221_hint_collection ht_cltn ON ht_cltn.hint_id = ht_log.hint_id LEFT JOIN b221_collection_log cltn_log ON cltn_log.collection_id = ht_cltn.collection_id
                                              GROUP BY ht_log.hint_id) unsorted_hints;")))
  
  return(output)
}