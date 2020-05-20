b221_process_display_info=function(is.freelancer = NULL, user.id = NULL, processed.rows = NULL, is.in.collection = NULL){
  
  setnames(processed.rows, c('id','clicked','country','product','intervention','assessment','url','official','comment','implementationdate','announcementdate','removaldate'),
           c('hint.id','relevance','implementer.name','product.group.name','intervention.type.name','assessment.name','url','is.official','comment','implementation.date','announcement.date','removal.date'))
  
  input.col.names = c('hint.id','implementer.name','url','is.official','assessment.name',
                      'product.group.name','intervention.type.name','comment','relevance','implementation.date','announcement.date','removal.date')
  multiple.values.permitted = c('implementer.name','product.group.name','intervention.type.name','collection.name')
  
  # if someone knows how to pass the column names as string into the ... of tidyr::unnest with multiple.values.permitted[1], be my guest
  # instead i manually pasted the names
  unnest1 = tidyr::unnest(processed.rows[,c('hint.id','implementer.name')], implementer.name) 
  unnest2 = tidyr::unnest(processed.rows[,c('hint.id','product.group.name')], product.group.name)
  unnest3 = tidyr::unnest(processed.rows[,c('hint.id','intervention.type.name')], intervention.type.name)
  
  processed.rows = as.data.frame(merge(merge(merge(processed.rows[,input.col.names[!input.col.names %in% multiple.values.permitted]], 
                                                   unnest1, by='hint.id', all.x = T),
                                             unnest2, by='hint.id', all.x = T),
                                       unnest3, by='hint.id', all.x = T))
  processed.rows$was.modified = 1
  processed.rows$in.collection = NA
  
  temp.changes.name=paste0("b221.temp.changes.data.",user.id)
  assign(temp.changes.name,processed.rows,envir=globalenv())
  
  gta_sql_get_value(paste0("DROP TABLE IF EXISTS ",gsub('\\.','_',temp.changes.name),";"),db.connection = 'pool')
  gta_sql_create_table(write.df=temp.changes.name,
                       append.existing = F,
                       table.prefix = '')
  #  ALTER TABLE b221_temp_changes_data_40 MODIFY comment TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
  if(is.freelancer==T){
    push.updates = paste0("/* FREELANCER UPLOAD */
                          SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log');
                          
                          INSERT INTO bt_classification_log(classification_id, user_id, hint_state_id, time_stamp)
                          SELECT DISTINCT @classification_id AS classification_id, ",user.id," AS user_id, (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - freelancer desk') AS hint_state_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp; 
                          
                          UPDATE b221_temp_changes_data_",user.id," changes
                          LEFT JOIN b221_hint_collection ON b221_hint_collection.hint_id = changes.hint_id 
                          SET changes.in_collection = (CASE WHEN b221_hint_collection.collection_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          CREATE INDEX idx_cmt ON b221_temp_changes_data_",user.id,"(comment(20));
                          
                          INSERT INTO b221_hint_comment_log(hint_id, user_id, comment, time_stamp)
                          SELECT DISTINCT changes.hint_id, ",user.id," AS user_id, comment, CONVERT_TZ(NOW(),'UTC','CET') AS time_stamp FROM b221_temp_changes_data_",user.id," changes
                          WHERE comment IS NOT NULL AND NOT EXISTS (SELECT NULL FROM b221_hint_comment_log cmt_log WHERE cmt_log.hint_id = changes.hint_id AND changes.comment = cmt_log.comment);
                          
                          DELETE b221_hint_assessment, b221_hint_product_group, b221_hint_intervention
                          FROM (SELECT * FROM b221_temp_changes_data_",user.id," WHERE in_collection = 0) changes
                          LEFT JOIN b221_hint_assessment ON changes.hint_id = b221_hint_assessment.hint_id
                          LEFT JOIN b221_hint_product_group ON changes.hint_id = b221_hint_product_group.hint_id
                          LEFT JOIN b221_hint_intervention ON changes.hint_id = b221_hint_intervention.hint_id
                          WHERE 1 = 1;
                          
                          INSERT INTO b221_hint_assessment(hint_id, classification_id, assessment_id, assessment_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, ass_list.assessment_id, NULL AS assessment_accepted, NULL AS validation_user 
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_assessment_list ass_list ON changes.assessment_name = ass_list.assessment_name
                          WHERE changes.in_collection = 0;
                          
                          INSERT INTO b221_hint_product_group(hint_id, classification_id, product_group_id, product_group_assessment, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, prod_grp_list.product_group_id, NULL AS product_group_assessment, NULL as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_product_group_list prod_grp_list ON prod_grp_list.product_group_name = changes.product_group_name
                          WHERE changes.in_collection = 0;
                          
                          INSERT INTO b221_hint_intervention(hint_id, classification_id, apparent_intervention_id, intervention_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, int_type_list.intervention_type_id, NULL AS product_group_assessment, NULL as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_intervention_type_list int_type_list ON int_type_list.intervention_type_name = changes.intervention_type_name
                          WHERE changes.in_collection = 0;
                          
                          CREATE INDEX src ON b221_temp_changes_data_",user.id," (url(300));
                          
                          DELETE bt_hint_jurisdiction, bt_hint_relevance, bt_hint_date FROM b221_temp_changes_data_",user.id," changes
                          LEFT JOIN bt_hint_jurisdiction ON changes.hint_id = bt_hint_jurisdiction.hint_id AND changes.in_collection = 0
                          LEFT JOIN bt_hint_relevance ON changes.hint_id = bt_hint_relevance.hint_id AND changes.in_collection = 0
                          LEFT JOIN bt_hint_date ON changes.hint_id = bt_hint_date.hint_id
                          WHERE 1 = 1;
                          
                          INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, jur_list.jurisdiction_id, NULL as jurisdiction_accepted, NULL as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_name = changes.implementer_name
                          WHERE changes.in_collection = 0;
                          
                          INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, jur_list.jurisdiction_id, NULL as jurisdiction_accepted, NULL as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_name = changes.implementer_name
                          WHERE changes.in_collection = 0;
                          
                          INSERT INTO bt_hint_date(hint_id, classification_id, date_type_id, `date`, date_accepted, validation_user)
                          SELECT DISTINCT * FROM
                          (SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'implementation') AS date_type_id, changes.implementation_date AS `date`, NULL as date_accepted, NULL as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          UNION 
                          SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'announcement') AS date_type_id, changes.announcement_date AS `date`, NULL as date_accepted, NULL as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          UNION 
                          SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'removal') AS date_type_id, changes.removal_date AS `date`, NULL as date_accepted, NULL as validation_user
                          FROM b221_temp_changes_data_",user.id," changes) new_dates
                          WHERE new_dates.`date` IS NOT NULL;
                          
                          INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_user)
                          SELECT changes_w_url_type.hint_id, bt_url_log.url_id, changes_w_url_type.url_type_id, @classification_id AS classification_id, NULL AS url_accepted, NULL AS validation_user FROM 
                          (SELECT DISTINCT changes.hint_id, changes.url, (CASE WHEN changes.is_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id
                          FROM b221_temp_changes_data_",user.id," changes) changes_w_url_type
                          JOIN bt_url_log ON changes_w_url_type.url = bt_url_log.url
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_url ht_url WHERE ht_url.hint_id = changes_w_url_type.hint_id AND ht_url.url_id = bt_url_log.url_id AND ht_url.url_type_id = changes_w_url_type.url_type_id);
                          
                          UPDATE bt_hint_url ht_url 
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON ht_url.hint_id = changed_hints.hint_id
                          JOIN bt_url_log ON bt_url_log.url_id = ht_url.url_id
                          LEFT JOIN (SELECT DISTINCT changes.hint_id, changes.url, (CASE WHEN changes.is_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id
                          FROM b221_temp_changes_data_",user.id," changes) changes_w_url_type
                          ON ht_url.hint_id = changes_w_url_type.hint_id AND changes_w_url_type.url = bt_url_log.url AND changes_w_url_type.url_type_id = ht_url.url_type_id
                          SET ht_url.url_accepted = (CASE WHEN changes_w_url_type.hint_id IS NOT NULL THEN NULL ELSE 0 END),
                          ht_url.classification_id = @classification_id;
                          
                          INSERT INTO bt_hint_relevance(hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, changes.relevance, NULL as relevance_probability, NULL as relevance_accepted, NULL as validation_user 
                          FROM b221_temp_changes_data_",user.id," changes
                          WHERE changes.in_collection = 0;
                          
                          UPDATE bt_hint_log
                          JOIN (SELECT DISTINCT b221_temp_changes_data_",user.id,".hint_id, relevance FROM b221_temp_changes_data_",user.id," WHERE in_collection = 0) changes ON changes.hint_id = bt_hint_log.hint_id
                          SET bt_hint_log.hint_state_id = (CASE WHEN changes.relevance = 1 THEN (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - editor desk') ELSE 
                          										(SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'trash bin - entered') END);")
  } else {
    processed.rows$was.modified = 1
    push.updates = paste0("/* give editor a classification_id only if they actually modified something that they put forward to process */
                          SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log');
                          INSERT INTO bt_classification_log(classification_id, user_id, hint_state_id, time_stamp)
                          SELECT DISTINCT @classification_id AS classification_id, ",user.id," AS user_id, (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - editor desk') AS hint_state_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp;
                          
                          CREATE INDEX idx_cmt ON b221_temp_changes_data_",user.id,"(comment(20));
                          
                          INSERT INTO b221_hint_comment_log(hint_id, user_id, comment, time_stamp)
                          SELECT DISTINCT changes.hint_id, ",user.id," AS user_id, comment, CONVERT_TZ(NOW(),'UTC','CET') AS time_stamp FROM b221_temp_changes_data_",user.id," changes
                          WHERE comment IS NOT NULL AND NOT EXISTS (SELECT NULL FROM b221_hint_comment_log cmt_log WHERE cmt_log.hint_id = changes.hint_id AND changes.comment = cmt_log.comment);
                          
                          /* case where new assessment from editor */
                          INSERT INTO b221_hint_assessment(hint_id, classification_id, assessment_id, assessment_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, ass_list.assessment_id , 1 AS assessment_accepted, ",user.id," AS validation_user 
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_assessment_list ass_list ON changes.assessment_name = ass_list.assessment_name
                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_assessment ht_ass WHERE ht_ass.hint_id = changes.hint_id AND ht_ass.assessment_id = ass_list.assessment_id);
                          
                          /* validate / refuse assessment from freelancer */
                          UPDATE b221_hint_assessment ht_ass
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON ht_ass.hint_id = changed_hints.hint_id
                          JOIN b221_assessment_list ass_list ON ht_ass.assessment_id = ass_list.assessment_id
                          LEFT JOIN (SELECT DISTINCT hint_id, assessment_name FROM b221_temp_changes_data_",user.id,") changes ON ht_ass.hint_id = changes.hint_id AND changes.assessment_name = ass_list.assessment_name
                          SET ht_ass.validation_user = ",user.id,",
                          ht_ass.assessment_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          /* case where new product group from editor */
                          INSERT INTO b221_hint_product_group(hint_id, classification_id, product_group_id, product_group_assessment, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, prod_grp_list.product_group_id, 1 AS product_group_assessment, ",user.id," as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_product_group_list prod_grp_list ON prod_grp_list.product_group_name = changes.product_group_name
                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_product_group prod_grp WHERE prod_grp.hint_id = changes.hint_id AND prod_grp_list.product_group_id = prod_grp.product_group_id);
                          
                          /* validate / refuse product group from freelancer */
                          UPDATE b221_hint_product_group prod_grp 
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON prod_grp.hint_id = changed_hints.hint_id
                          JOIN b221_product_group_list prod_grp_list ON prod_grp_list.product_group_id = prod_grp.product_group_id
                          LEFT JOIN (SELECT DISTINCT hint_id, product_group_name FROM b221_temp_changes_data_",user.id,") changes ON changes.hint_id = prod_grp.hint_id AND changes.product_group_name = prod_grp_list.product_group_name
                          SET prod_grp.validation_user = ",user.id,",
                          prod_grp.product_group_assessment = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          /* case where new intervention type from editor */
                          INSERT INTO b221_hint_intervention(hint_id, classification_id, apparent_intervention_id, intervention_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, int_type_list.intervention_type_id, 1 AS product_group_assessment, ",user.id," as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_intervention_type_list int_type_list ON int_type_list.intervention_type_name = changes.intervention_type_name
                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_intervention ht_int WHERE ht_int.hint_id = changes.hint_id AND int_type_list.intervention_type_id = ht_int.apparent_intervention_id);
                          
                          /* validate / refuse intervention type from freelancer */
                          UPDATE b221_hint_intervention ht_int 
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON ht_int.hint_id = changed_hints.hint_id
                          JOIN b221_intervention_type_list int_type_list ON int_type_list.intervention_type_id = ht_int.apparent_intervention_id
                          LEFT JOIN (SELECT DISTINCT hint_id, intervention_type_name FROM b221_temp_changes_data_",user.id,") changes ON ht_int.hint_id = changes.hint_id AND changes.intervention_type_name = int_type_list.intervention_type_name
                          SET ht_int.validation_user = ",user.id,", 
                          ht_int.intervention_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          CREATE INDEX src ON b221_temp_changes_data_",user.id," (url(300));
                          /* assumption here is that there is only 1 url which is pulled which is also immutable, i just need to go say yes/no to the existing entry proposed by my freelancer 
                          editor can only choose a different type 
                          */
                          INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_user)
                          SELECT changes_w_url_type.hint_id, bt_url_log.url_id, changes_w_url_type.url_type_id, @classification_id AS classification_id, 1 AS url_accepted, ",user.id," AS validation_user FROM 
                          (SELECT DISTINCT changes.hint_id, changes.url, (CASE WHEN changes.is_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id
                          FROM b221_temp_changes_data_",user.id," changes) changes_w_url_type
                          JOIN bt_url_log ON changes_w_url_type.url = bt_url_log.url
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_url ht_url WHERE ht_url.hint_id = changes_w_url_type.hint_id AND ht_url.url_id = bt_url_log.url_id AND ht_url.url_type_id = changes_w_url_type.url_type_id);
                          
                          UPDATE bt_hint_url ht_url 
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON ht_url.hint_id = changed_hints.hint_id
                          JOIN bt_url_log ON bt_url_log.url_id = ht_url.url_id
                          LEFT JOIN (SELECT DISTINCT changes.hint_id, changes.url, (CASE WHEN changes.is_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id
                          	   FROM b221_temp_changes_data_",user.id," changes) changes_w_url_type
                          ON ht_url.hint_id = changes_w_url_type.hint_id AND changes_w_url_type.url = bt_url_log.url AND changes_w_url_type.url_type_id = ht_url.url_type_id
                          SET ht_url.validation_user = ",user.id,", 
                          ht_url.url_accepted = (CASE WHEN changes_w_url_type.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          /* SAME APPROACH WITH BT_HINT_JURISDICTION */
                          INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, jur_list.jurisdiction_id, 1 AS jurisdiction_accepted, ",user.id," as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN gta_jurisdiction_list jur_list ON changes.implementer_name = jur_list.jurisdiction_name
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_jurisdiction ht_jur WHERE ht_jur.hint_id = changes.hint_id AND jur_list.jurisdiction_id = ht_jur.jurisdiction_id);
                          
                          UPDATE bt_hint_jurisdiction ht_jur 
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON ht_jur.hint_id = changed_hints.hint_id
                          JOIN gta_jurisdiction_list jur_list ON ht_jur.jurisdiction_id = jur_list.jurisdiction_id
                          LEFT JOIN (SELECT DISTINCT hint_id, implementer_name FROM b221_temp_changes_data_",user.id,") changes ON ht_jur.hint_id = changes.hint_id AND changes.implementer_name = jur_list.jurisdiction_name
                          SET ht_jur.validation_user = ",user.id,", 
                          ht_jur.jurisdiction_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          /* AND AGAIN WITH BT_HINT_RELEVANCE */
                          INSERT INTO bt_hint_relevance(hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, changes.relevance, NULL as relevance_probability, 1 AS relevance_accepted, ",user.id," as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_relevance ht_rel WHERE ht_rel.hint_id = changes.hint_id AND ht_rel.relevance = changes.relevance);
                          
                          UPDATE bt_hint_relevance ht_rel
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON ht_rel.hint_id = changed_hints.hint_id
                          LEFT JOIN (SELECT DISTINCT hint_id, relevance FROM b221_temp_changes_data_",user.id,") changes ON ht_rel.hint_id = changes.hint_id AND changes.relevance = ht_rel.relevance
                          SET ht_rel.validation_user = ",user.id,", 
                          ht_rel.relevance_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          /* AND AGAIN WITH BT_HINT_DATE */
                          INSERT INTO bt_hint_date(hint_id, classification_id, date_type_id, `date`, date_accepted, validation_user)
                          SELECT DISTINCT * FROM
                          (SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'implementation') AS date_type_id, changes.implementation_date AS `date`, NULL as date_accepted, NULL as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          UNION 
                          SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'announcement') AS date_type_id, changes.announcement_date AS `date`, NULL as date_accepted, NULL as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          UNION 
                          SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'removal') AS date_type_id, changes.removal_date AS `date`, NULL as date_accepted, NULL as validation_user
                          FROM b221_temp_changes_data_",user.id," changes) new_dates
                          WHERE new_dates.`date` IS NOT NULL
                          AND NOT EXISTS (SELECT NULL FROM bt_hint_date ht_date WHERE ht_date.hint_id = new_dates.hint_id AND ht_date.`date` = new_dates.`date` AND ht_date.date_type_id = new_dates.date_type_id);
                          
                          UPDATE bt_hint_date ht_date
                          JOIN (SELECT DISTINCT hint_id FROM b221_temp_changes_data_",user.id,") changed_hints ON ht_date.hint_id = changed_hints.hint_id
                          LEFT JOIN (SELECT DISTINCT * FROM
                          		(SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'implementation') AS date_type_id, changes.implementation_date AS `date`, NULL as date_accepted, NULL as validation_user
                          		FROM b221_temp_changes_data_",user.id," changes
                          		UNION 
                          		SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'announcement') AS date_type_id, changes.announcement_date AS `date`, NULL as date_accepted, NULL as validation_user
                          		FROM b221_temp_changes_data_",user.id," changes
                          		UNION 
                          		SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'removal') AS date_type_id, changes.removal_date AS `date`, NULL as date_accepted, NULL as validation_user
                          		FROM b221_temp_changes_data_",user.id," changes) changes
                          		WHERE changes.`date` IS NOT NULL) new_dates ON ht_date.hint_id = new_dates.hint_id AND ht_date.`date` = new_dates.`date` AND ht_date.date_type_id = new_dates.date_type_id
                          SET ht_date.validation_user = ",user.id,", 
                          	ht_date.date_accepted = (CASE WHEN new_dates.hint_id IS NOT NULL THEN 1 ELSE 0 END);	
                          
                          UPDATE bt_hint_log
                          JOIN (SELECT DISTINCT b221_temp_changes_data_",user.id,".hint_id, is_official, relevance FROM b221_temp_changes_data_",user.id,") changes ON changes.hint_id = bt_hint_log.hint_id
                          SET bt_hint_log.hint_state_id = (CASE WHEN (changes.is_official = 0 AND changes.relevance = 1) THEN (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'OSC - freelancer desk') 
                          	  WHEN (changes.is_official = 1 AND changes.relevance = 1) THEN (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'BT - ready for dispatch') 
                          	  ELSE (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'trash bin - fully processed') END);
                          
                          /* auto graduation query for collections */
                          UPDATE bt_hint_log
                          JOIN 
                          (SELECT b221_hint_collection.hint_id, changed_collection.max_state AS new_state FROM b221_hint_collection
                          JOIN
                          (SELECT DISTINCT locate_collection_hints.collection_id,
                          MAX(hint_state_id) max_state
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_hint_collection allocate_collection ON changes.hint_id = allocate_collection.hint_id
                          JOIN b221_hint_collection locate_collection_hints ON allocate_collection.collection_id = locate_collection_hints.collection_id
                          JOIN bt_hint_log ON locate_collection_hints.hint_id = bt_hint_log.hint_id
                          GROUP BY locate_collection_hints.collection_id) changed_collection ON b221_hint_collection.collection_id = changed_collection.collection_id) new_hint_states ON bt_hint_log.hint_id = new_hint_states.hint_id
                          SET bt_hint_log.hint_state_id = new_hint_states.new_state;")
  }
  
  gta_sql_multiple_queries(push.updates, output.queries = 1, show.time = T, db.connection = 'pool')
  #gta_sql_get_value(paste0("DROP TABLE IF EXISTS ",gsub('\\.','_',temp.changes.name),";"),db.connection = 'pool')
  
}

# last statement of editor where states are updated needs to update to correct state if hint in collection, this is not yet done