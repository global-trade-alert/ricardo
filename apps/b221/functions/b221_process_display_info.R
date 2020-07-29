b221_process_display_info=function(is.freelancer = NULL, user.id = NULL, processed.rows = NULL, is.in.collection = NULL, text.modifiable = NULL){
  
  # could be fancier and make sure that new submissions are not identical to previous ones but i have left this aside for now, it would be a where not exists statement which groups the attributes and compares the highest validation_classification with the new submission
  setnames(processed.rows, c('id','clicked','country','product','intervention','assessment','url','official','comment','implementationdate','announcementdate','removaldate'),
           c('hint.id','relevance','implementer.name','product.group.name','intervention.type.name','assessment.name','url','is.official','comment','implementation.date','announcement.date','removal.date'))
  
  input.col.names = c('hint.id','implementer.name','url','is.official','assessment.name',
                      'product.group.name','intervention.type.name','comment','relevance','implementation.date','announcement.date','removal.date', 'discard.reason', 'discard.reason.comment')
  multiple.values.permitted = c('implementer.name','product.group.name','intervention.type.name','collection.name', 'discard.reason')
  
  if(text.modifiable == T) input.col.names = c(input.col.names, 'title','hint.description')
  
  # if someone knows how to pass the column names as string into the ... of tidyr::unnest with multiple.values.permitted[1], be my guest
  # instead i manually pasted the names
  unnest1 = tidyr::unnest(processed.rows[,c('hint.id','implementer.name')], implementer.name) 
  unnest2 = tidyr::unnest(processed.rows[,c('hint.id','product.group.name')], product.group.name)
  unnest3 = tidyr::unnest(processed.rows[,c('hint.id','intervention.type.name')], intervention.type.name)
  unnest4 = tidyr::unnest(processed.rows[,c('hint.id','discard.reason')], discard.reason)
  
  processed.rows = as.data.frame(merge(merge(merge(merge(processed.rows[,input.col.names[!input.col.names %in% multiple.values.permitted]], 
                                                   unnest1, by='hint.id', all.x = T),
                                             unnest2, by='hint.id', all.x = T),
                                       unnest3, by='hint.id', all.x = T),
                                     unnest4, by='hint.id', all.x = T))
  processed.rows$was.modified = 1
  processed.rows$in.collection = NA
  
  temp.changes.name=paste0("b221.temp.changes.data.",user.id)
  assign(temp.changes.name,processed.rows,envir=globalenv())
  
  test_processed.rows <<- processed.rows

  gta_sql_get_value(paste0("DROP TABLE IF EXISTS ",gsub('\\.','_',temp.changes.name),";"),db.connection = 'pool')
  gta_sql_create_table(write.df=temp.changes.name,
                       append.existing = F,
                       table.prefix = '')

  if(is.freelancer==T){
    push.updates = paste0("/* FREELANCER UPLOAD */
                          SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log' AND table_schema=DATABASE());
                          
                          INSERT INTO bt_classification_log(classification_id, user_id, hint_state_id, time_stamp)
                          SELECT DISTINCT @classification_id AS classification_id, ",user.id," AS user_id, (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - freelancer desk') AS hint_state_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp; 
                          
                          UPDATE b221_temp_changes_data_",user.id," changes
                          LEFT JOIN b221_hint_collection ON b221_hint_collection.hint_id = changes.hint_id 
                          SET changes.in_collection = (CASE WHEN b221_hint_collection.collection_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          CREATE INDEX idx_cmt ON b221_temp_changes_data_",user.id,"(comment(20));
                          
                          INSERT INTO b221_hint_comment_log(hint_id, user_id, comment, time_stamp)
                          SELECT DISTINCT changes.hint_id, ",user.id," AS user_id, comment, CONVERT_TZ(NOW(),'UTC','CET') AS time_stamp FROM b221_temp_changes_data_",user.id," changes
                          WHERE comment IS NOT NULL AND NOT EXISTS (SELECT NULL FROM b221_hint_comment_log cmt_log WHERE cmt_log.hint_id = changes.hint_id AND changes.comment = cmt_log.comment);
                          
                          DELETE b221_hint_assessment, b221_hint_product_group, b221_hint_intervention, bt_hint_jurisdiction, bt_hint_relevance, bt_hint_date, bt_hint_url, bt_hint_discard_reason
                          FROM (SELECT * FROM b221_temp_changes_data_",user.id,") changes
                          LEFT JOIN b221_hint_assessment ON changes.hint_id = b221_hint_assessment.hint_id AND b221_hint_assessment.validation_classification IS NULL AND changes.in_collection = 0
                          LEFT JOIN b221_hint_product_group ON changes.hint_id = b221_hint_product_group.hint_id AND b221_hint_product_group.validation_classification IS NULL AND changes.in_collection = 0
                          LEFT JOIN b221_hint_intervention ON changes.hint_id = b221_hint_intervention.hint_id AND b221_hint_intervention.validation_classification IS NULL AND changes.in_collection = 0
                          LEFT JOIN bt_hint_jurisdiction ON changes.hint_id = bt_hint_jurisdiction.hint_id AND bt_hint_jurisdiction.validation_classification IS NULL AND changes.in_collection = 0
                          LEFT JOIN bt_hint_relevance ON changes.hint_id = bt_hint_relevance.hint_id AND bt_hint_relevance.validation_classification IS NULL AND changes.in_collection = 0
                          LEFT JOIN bt_hint_date ON changes.hint_id = bt_hint_date.hint_id AND bt_hint_date.validation_classification IS NULL
                          LEFT JOIN bt_hint_url ON changes.hint_id = bt_hint_url.hint_id AND bt_hint_url.validation_classification IS NULL
                          LEFT JOIN bt_hint_discard_reason ON changes.hint_id = bt_hint_discard_reason.hint_id AND bt_hint_discard_reason.validation_classification IS NULL
                          WHERE 1 = 1;

                          INSERT INTO b221_hint_assessment(hint_id, classification_id, assessment_id, assessment_accepted, validation_classification, confirm_status)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, ass_list.assessment_id, NULL AS assessment_accepted, NULL AS validation_classification, 0 AS confirm_status 
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_assessment_list ass_list ON changes.assessment_name = ass_list.assessment_name
                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_assessment ht_ass WHERE ht_ass.hint_id = changes.hint_id AND ht_ass.assessment_id = ass_list.assessment_id AND ht_ass.validation_classification IS NULL)
                          AND changes.in_collection = 0;
                          
                          INSERT INTO b221_hint_product_group(hint_id, classification_id, product_group_id, product_group_assessment, validation_classification, confirm_status)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, prod_grp_list.product_group_id, NULL AS product_group_assessment, NULL as validation_classification, 0 AS confirm_status
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_product_group_list prod_grp_list ON changes.product_group_name = prod_grp_list.product_group_name
                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_product_group prod_grp WHERE prod_grp.hint_id = changes.hint_id AND prod_grp.product_group_id = prod_grp_list.product_group_id AND prod_grp.validation_classification IS NULL)
                          AND changes.in_collection = 0;
                          
                          INSERT INTO b221_hint_intervention(hint_id, classification_id, apparent_intervention_id, intervention_accepted, validation_classification, confirm_status)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, int_list.intervention_type_id, NULL AS intervention_accepted, NULL as validation_classification, 0 AS confirm_status
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_intervention_type_list int_list ON changes.intervention_type_name = int_list.intervention_type_name
                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_intervention ht_int WHERE ht_int.hint_id = changes.hint_id AND ht_int.apparent_intervention_id = int_list.intervention_type_id AND ht_int.validation_classification IS NULL)
                          AND changes.in_collection = 0;
                          
                          CREATE INDEX src ON b221_temp_changes_data_",user.id," (url(300));
                          
                          INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_classification, confirm_status)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, jur_list.jurisdiction_id, NULL AS jurisdiction_accepted, NULL as validation_classification, 0 AS confirm_status
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN gta_jurisdiction_list jur_list ON changes.implementer_name = jur_list.jurisdiction_name
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_jurisdiction ht_jur WHERE ht_jur.hint_id = changes.hint_id AND ht_jur.jurisdiction_id = jur_list.jurisdiction_id AND ht_jur.validation_classification IS NULL)
                          AND changes.in_collection = 0;
                          
                          INSERT INTO bt_hint_date(hint_id, classification_id, date_type_id, `date`, date_accepted, validation_classification, confirm_status)
                          SELECT DISTINCT * FROM
                          (SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'implementation') AS date_type_id, changes.implementation_date AS `date`, NULL as date_accepted, NULL as validation_classification, 0 AS confirm_status
                          FROM b221_temp_changes_data_",user.id," changes
                          UNION 
                          SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'announcement') AS date_type_id, changes.announcement_date AS `date`, NULL as date_accepted, NULL as validation_classification, 0 AS confirm_status
                          FROM b221_temp_changes_data_",user.id," changes
                          UNION 
                          SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'removal') AS date_type_id, changes.removal_date AS `date`, NULL as date_accepted, NULL as validation_classification, 0 AS confirm_status
                          FROM b221_temp_changes_data_",user.id," changes) new_dates
                          WHERE new_dates.`date` IS NOT NULL
                          AND NOT EXISTS (SELECT NULL FROM bt_hint_date ht_date WHERE ht_date.hint_id = new_dates.hint_id AND ht_date.`date` = new_dates.`date` AND ht_date.date_type_id = new_dates.date_type_id AND ht_date.validation_classification IS NULL)
                          AND changes.in_collection = 0;
                          
                          INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_classification, confirm_status)
                          SELECT changes_w_url_type.hint_id, bt_url_log.url_id, changes_w_url_type.url_type_id, @classification_id AS classification_id, NULL AS url_accepted, NULL AS validation_classification, 0 AS confirm_status
                          FROM (SELECT DISTINCT changes.hint_id, changes.url, (CASE WHEN changes.is_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id
                          FROM b221_temp_changes_data_",user.id," changes) changes_w_url_type
                          JOIN bt_url_log ON changes_w_url_type.url = bt_url_log.url
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_url ht_url WHERE ht_url.hint_id = changes_w_url_type.hint_id AND ht_url.url_id = bt_url_log.url_id AND ht_url.url_type_id = changes_w_url_type.url_type_id AND ht_url.validation_classification IS NULL)
                          AND changes.in_collection = 0;
                          
                          INSERT INTO bt_hint_relevance(hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_classification, confirm_status)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, changes.relevance, NULL as relevance_probability, NULL as relevance_accepted, NULL as validation_classification, 0 AS confirm_status 
                          FROM b221_temp_changes_data_",user.id," changes
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_relevance ht_rel WHERE ht_rel.hint_id = changes.hint_id AND ht_rel.relevance = changes.relevance AND ht_rel.validation_classification IS NULL)
                          AND changes.in_collection = 0;
                          
                          INSERT INTO bt_hint_discard_reason (hint_id, classification_id, discard_reason_id , discard_reason_comment, reason_accepted, validation_classification, confirm_status)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, dis_list.discard_reason_id, changes.discard_reason_comment, 
                          NULL AS reason_accepted, NULL AS validation_classification, 0 AS confirm_status
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN bt_discard_reason_list dis_list ON changes.discard_reason = dis_list.discard_reason_name 
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_discard_reason dis_hint WHERE dis_hint.hint_id = changes.hint_id AND dis_hint.discard_reason_id = dis_list.discard_reason_id AND dis_hint.discard_reason_comment COLLATE utf8mb4_general_ci <=> changes.discard_reason_comment AND dis_hint.validation_classification IS NULL)
                          AND changes.in_collection = 0;
                          
                          UPDATE bt_hint_log
                          JOIN (SELECT DISTINCT b221_temp_changes_data_",user.id,".hint_id, relevance FROM b221_temp_changes_data_",user.id," WHERE in_collection = 0) changes ON changes.hint_id = bt_hint_log.hint_id
                          SET bt_hint_log.hint_state_id = (CASE WHEN changes.relevance = 1 THEN (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - editor desk') ELSE 
                          (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'trash bin - entered') END);")
  } else {
    processed.rows$was.modified = 1
    push.updates = paste0("/* EDITOR UPLOAD */
                          SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log' AND table_schema=DATABASE());
                          
                          INSERT INTO bt_classification_log(classification_id, user_id, hint_state_id, time_stamp)
                          SELECT DISTINCT @classification_id AS classification_id, ",user.id," AS user_id, (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - editor desk') AS hint_state_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp; 
                          
                          UPDATE b221_temp_changes_data_",user.id," changes
                          LEFT JOIN b221_hint_collection ON b221_hint_collection.hint_id = changes.hint_id 
                          SET changes.in_collection = (CASE WHEN b221_hint_collection.collection_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          CREATE INDEX idx_cmt ON b221_temp_changes_data_",user.id,"(comment(20));
                          
                          INSERT INTO b221_hint_comment_log(hint_id, user_id, comment, time_stamp)
                          SELECT DISTINCT changes.hint_id, ",user.id," AS user_id, comment, CONVERT_TZ(NOW(),'UTC','CET') AS time_stamp FROM b221_temp_changes_data_",user.id," changes
                          WHERE comment IS NOT NULL AND NOT EXISTS (SELECT NULL FROM b221_hint_comment_log cmt_log WHERE cmt_log.hint_id = changes.hint_id AND changes.comment = cmt_log.comment);
                          
                          INSERT INTO b221_hint_assessment(hint_id, classification_id, assessment_id, assessment_accepted, validation_classification, confirm_status)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, ass_list.assessment_id, NULL AS assessment_accepted, NULL AS validation_classification, 0 AS confirm_status
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_assessment_list ass_list ON changes.assessment_name = ass_list.assessment_name
                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_assessment ht_ass WHERE ht_ass.hint_id = changes.hint_id AND ht_ass.assessment_id = ass_list.assessment_id AND ht_ass.validation_classification IS NULL)
                          AND changes.in_collection = 0;
                          
                          UPDATE b221_hint_assessment ht_ass
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON ht_ass.hint_id = changed_hints.hint_id AND ht_ass.validation_classification IS NULL
                          JOIN b221_assessment_list ass_list ON ht_ass.assessment_id = ass_list.assessment_id
                          LEFT JOIN (SELECT DISTINCT hint_id, assessment_name FROM b221_temp_changes_data_",user.id,") changes ON ht_ass.hint_id = changes.hint_id AND changes.assessment_name = ass_list.assessment_name AND ht_ass.validation_classification IS NULL
                          SET ht_ass.validation_classification = @classification_id,
                          ht_ass.assessment_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          INSERT INTO b221_hint_product_group(hint_id, classification_id, product_group_id, product_group_assessment, validation_classification, confirm_status)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, prod_grp_list.product_group_id, NULL AS product_group_assessment, NULL as validation_classification, 0 AS confirm_status
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_product_group_list prod_grp_list ON changes.product_group_name = prod_grp_list.product_group_name
                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_product_group prod_grp WHERE prod_grp.hint_id = changes.hint_id AND prod_grp.product_group_id = prod_grp_list.product_group_id AND prod_grp.validation_classification IS NULL)
                          AND changes.in_collection = 0;
                          
                          UPDATE b221_hint_product_group ht_prod
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON ht_prod.hint_id = changed_hints.hint_id AND ht_prod.validation_classification IS NULL
                          JOIN b221_product_group_list prod_list ON ht_prod.product_group_id = prod_list.product_group_id
                          LEFT JOIN (SELECT DISTINCT hint_id, product_group_name FROM b221_temp_changes_data_",user.id,") changes ON ht_prod.hint_id = changes.hint_id AND changes.product_group_name = prod_list.product_group_name AND ht_prod.validation_classification IS NULL
                          SET ht_prod.validation_classification = @classification_id,
                          ht_prod.product_group_assessment = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          INSERT INTO b221_hint_intervention(hint_id, classification_id, apparent_intervention_id, intervention_accepted, validation_classification, confirm_status)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, int_list.intervention_type_id, NULL AS intervention_accepted, NULL as validation_classification, 0 AS confirm_status
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_intervention_type_list int_list ON changes.intervention_type_name = int_list.intervention_type_name
                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_intervention ht_int WHERE ht_int.hint_id = changes.hint_id AND ht_int.apparent_intervention_id = int_list.intervention_type_id AND ht_int.validation_classification IS NULL)
                          AND changes.in_collection = 0;
                          
                          UPDATE b221_hint_intervention ht_int 
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON ht_int.hint_id = changed_hints.hint_id AND ht_int.validation_classification IS NULL
                          JOIN b221_intervention_type_list int_type_list ON int_type_list.intervention_type_id = ht_int.apparent_intervention_id
                          LEFT JOIN (SELECT DISTINCT hint_id, intervention_type_name FROM b221_temp_changes_data_",user.id,") changes ON ht_int.hint_id = changes.hint_id AND changes.intervention_type_name = int_type_list.intervention_type_name AND ht_int.validation_classification IS NULL
                          SET ht_int.validation_classification = @classification_id, 
                          ht_int.intervention_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          CREATE INDEX src ON b221_temp_changes_data_",user.id," (url(300));
                          
                          INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_classification, confirm_status)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, jur_list.jurisdiction_id, NULL AS jurisdiction_accepted, NULL as validation_classification, 0 AS confirm_status
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN gta_jurisdiction_list jur_list ON changes.implementer_name = jur_list.jurisdiction_name
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_jurisdiction ht_jur WHERE ht_jur.hint_id = changes.hint_id AND ht_jur.jurisdiction_id = jur_list.jurisdiction_id AND ht_jur.validation_classification IS NULL)
                          AND changes.in_collection = 0;
                          
                          UPDATE bt_hint_jurisdiction ht_jur 
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON ht_jur.hint_id = changed_hints.hint_id AND ht_jur.validation_classification IS NULL
                          JOIN gta_jurisdiction_list jur_list ON ht_jur.jurisdiction_id = jur_list.jurisdiction_id
                          LEFT JOIN (SELECT DISTINCT hint_id, implementer_name FROM b221_temp_changes_data_",user.id,") changes ON ht_jur.hint_id = changes.hint_id AND changes.implementer_name = jur_list.jurisdiction_name AND ht_jur.validation_classification IS NULL
                          SET ht_jur.validation_classification = @classification_id, 
                          ht_jur.jurisdiction_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          INSERT INTO bt_hint_date(hint_id, classification_id, date_type_id, `date`, date_accepted, validation_classification, confirm_status)
                          SELECT DISTINCT hint_id, classification_id, date_type_id, `date`, date_accepted, validation_classification, confirm_status FROM
                          (SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'implementation') AS date_type_id, changes.implementation_date AS `date`, NULL as date_accepted, NULL as validation_classification, 0 AS confirm_status, changes.in_collection AS in_collection
                          FROM b221_temp_changes_data_",user.id," changes
                          UNION 
                          SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'announcement') AS date_type_id, changes.announcement_date AS `date`, NULL as date_accepted, NULL as validation_classification, 0 AS confirm_status, changes.in_collection AS in_collection
                          FROM b221_temp_changes_data_",user.id," changes
                          UNION 
                          SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'removal') AS date_type_id, changes.removal_date AS `date`, NULL as date_accepted, NULL as validation_classification, 0 AS confirm_status, changes.in_collection AS in_collection
                          FROM b221_temp_changes_data_",user.id," changes) new_dates
                          WHERE new_dates.`date` IS NOT NULL
                          AND NOT EXISTS (SELECT NULL FROM bt_hint_date ht_date WHERE ht_date.hint_id = new_dates.hint_id AND ht_date.`date` = new_dates.`date` AND ht_date.date_type_id = new_dates.date_type_id AND ht_date.validation_classification IS NULL)
                          AND new_dates.in_collection = 0;
                          
                          UPDATE bt_hint_date ht_date
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON ht_date.hint_id = changed_hints.hint_id AND ht_date.validation_classification IS NULL
                          LEFT JOIN (SELECT DISTINCT * FROM
                          (SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'implementation') AS date_type_id, changes.implementation_date AS `date`, NULL as date_accepted, NULL as validation_classification
                          FROM b221_temp_changes_data_",user.id," changes
                          UNION 
                          SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'announcement') AS date_type_id, changes.announcement_date AS `date`, NULL as date_accepted, NULL as validation_classification
                          FROM b221_temp_changes_data_",user.id," changes
                          UNION 
                          SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'removal') AS date_type_id, changes.removal_date AS `date`, NULL as date_accepted, NULL as validation_classification
                          FROM b221_temp_changes_data_",user.id," changes) changes
                          WHERE changes.`date` IS NOT NULL) new_dates ON ht_date.hint_id = new_dates.hint_id AND ht_date.`date` = new_dates.`date` AND ht_date.date_type_id = new_dates.date_type_id AND ht_date.validation_classification IS NULL
                          SET ht_date.validation_classification = @classification_id, 
                          ht_date.date_accepted = (CASE WHEN new_dates.hint_id IS NOT NULL THEN 1 ELSE 0 END);                          
                          
                          INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_classification, confirm_status)
                          SELECT changes_w_url_type.hint_id, bt_url_log.url_id, changes_w_url_type.url_type_id, @classification_id AS classification_id, NULL AS url_accepted, NULL AS validation_classification, 0 AS confirm_status
                          FROM (SELECT DISTINCT changes.hint_id, changes.url, (CASE WHEN changes.is_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id, changes.in_collection AS in_collection
                          FROM b221_temp_changes_data_",user.id," changes) changes_w_url_type
                          JOIN bt_url_log ON changes_w_url_type.url = bt_url_log.url
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_url ht_url WHERE ht_url.hint_id = changes_w_url_type.hint_id AND ht_url.url_id = bt_url_log.url_id AND ht_url.url_type_id = changes_w_url_type.url_type_id AND ht_url.validation_classification IS NULL)
                          AND changes_w_url_type.in_collection = 0;
                          
                          UPDATE bt_hint_url ht_url 
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON ht_url.hint_id = changed_hints.hint_id
                          JOIN bt_url_log ON bt_url_log.url_id = ht_url.url_id
                          LEFT JOIN (SELECT DISTINCT changes.hint_id, changes.url, (CASE WHEN changes.is_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id
                           FROM b221_temp_changes_data_",user.id," changes) changes_w_url_type
                          ON ht_url.hint_id = changes_w_url_type.hint_id AND changes_w_url_type.url = bt_url_log.url AND changes_w_url_type.url_type_id = ht_url.url_type_id
                          SET ht_url.validation_classification = @classification_id, ht_url.url_accepted = (CASE WHEN changes_w_url_type.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                                                  
                          INSERT INTO bt_hint_relevance(hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_classification, confirm_status)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, changes.relevance, NULL as relevance_probability, NULL as relevance_accepted, NULL as validation_classification, 0 AS confirm_status 
                          FROM b221_temp_changes_data_",user.id," changes
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_relevance ht_rel WHERE ht_rel.hint_id = changes.hint_id AND ht_rel.relevance = changes.relevance AND ht_rel.validation_classification IS NULL)
                          AND changes.in_collection = 0;
                          
                          UPDATE bt_hint_relevance ht_rel
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON ht_rel.hint_id = changed_hints.hint_id AND ht_rel.validation_classification IS NULL
                          LEFT JOIN (SELECT DISTINCT hint_id, relevance FROM b221_temp_changes_data_",user.id,") changes ON ht_rel.hint_id = changes.hint_id AND changes.relevance = ht_rel.relevance AND ht_rel.validation_classification IS NULL
                          SET ht_rel.validation_classification = @classification_id, 
                          ht_rel.relevance_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          INSERT INTO bt_hint_discard_reason (hint_id, classification_id, discard_reason_id, discard_reason_comment, reason_accepted, validation_classification, confirm_status)
                          SELECT DISTINCT changes.hint_id, @classification_id as classification_id,
                          bdr.discard_reason_id, changes.discard_reason_comment, 
                          NULL AS reason_accepted, NULL AS validation_classification, 0 AS confirm_status
                          FROM b221_temp_changes_data_",user.id," AS changes
                          JOIN bt_discard_reason_list bdr ON changes.discard_reason COLLATE utf8mb4_general_ci = bdr.discard_reason_name
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_discard_reason bt_dis WHERE bt_dis.hint_id = changes.hint_id AND bt_dis.discard_reason_id = bdr.discard_reason_id AND bt_dis.discard_reason_comment COLLATE utf8mb4_general_ci <=> changes.discard_reason_comment AND bt_dis.validation_classification IS NULL)
                          AND changes.in_collection = 0;
                          
                          UPDATE bt_hint_discard_reason bt_dis
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON bt_dis.hint_id = changed_hints.hint_id AND bt_dis.validation_classification IS NULL
                          JOIN bt_discard_reason_list dis_list ON bt_dis.discard_reason_id = dis_list.discard_reason_id
                          LEFT JOIN (SELECT DISTINCT hint_id, discard_reason, discard_reason_comment FROM b221_temp_changes_data_",user.id,") changes ON bt_dis.hint_id = changes.hint_id AND bt_dis.discard_reason_comment COLLATE utf8mb4_general_ci <=> changes.discard_reason_comment AND changes.discard_reason COLLATE utf8mb4_general_ci = dis_list.discard_reason_name AND bt_dis.validation_classification IS NULL
                          SET bt_dis.validation_classification = @classification_id,
                          bt_dis.reason_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          UPDATE bt_hint_log
                          JOIN (SELECT DISTINCT b221_temp_changes_data_",user.id,".hint_id, relevance FROM b221_temp_changes_data_",user.id," WHERE in_collection = 0) changes ON changes.hint_id = bt_hint_log.hint_id
                          SET bt_hint_log.hint_state_id = (CASE WHEN changes.relevance = 1 THEN (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - editor desk') ELSE 
                          (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'trash bin - entered') END);")
    
    
    if(text.modifiable == T){
      
      push.updates = paste0(push.updates, 
                           "INSERT INTO bt_hint_text(hint_id, hint_title, hint_description, language_id, classification_id, description_accepted, validation_classification)
                            SELECT DISTINCT changes.hint_id, changes.title AS hint_title, changes.hint_description AS hint_description, 1 AS language_id, @classification_id AS classification_id, 1 AS description_accepted, @classification_id as validation_classification
                            FROM b221_temp_changes_data_",user.id," changes
                            WHERE NOT EXISTS (SELECT NULL FROM (SELECT bt_hint_text.hint_id, bt_hint_text.hint_description, bt_hint_text.hint_title FROM bt_hint_text JOIN 
                            (SELECT bt_hint_text.hint_id, MAX(bt_hint_text.validation_classification) AS newest_classification 
                            FROM bt_hint_text GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_text.hint_id 
                            AND bt_hint_text.language_id = 1 AND newest_classification.newest_classification <=> bt_hint_text.validation_classification) ht_txt 
                            WHERE ht_txt.hint_id = changes.hint_id AND ht_txt.hint_description COLLATE utf8mb4_general_ci = changes.hint_description 
                            AND ht_txt.hint_title COLLATE utf8mb4_general_ci = changes.title);")
      
    }
    
  }
  
  gta_sql_multiple_queries(push.updates, output.queries = 1, show.time = T, db.connection = 'pool')
  gta_sql_get_value(paste0("DROP TABLE IF EXISTS ",gsub('\\.','_',temp.changes.name),";"),db.connection = 'pool')
  
}
