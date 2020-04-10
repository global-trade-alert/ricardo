b221_process_display_info=function(is.freelancer = NULL, user.id = NULL, processed.rows = NULL){
  # this code was written quickly, code check needs to be attentive, also high chance i forgot to replace a "1 as validation_user/user_id or validation_user/user_id = 1" somewhere with the R var user.id
  
  # not sure what sort of dataframe is easiest to compile inside the app.. 
  # i will assume you create a nested dataframe with lists as the columns
  # i.e. if hint_id 1 is matched to product group 1 and 2, then i would have a row with hint_id = 1 and product.group = c(1,2)
  # for example: 
  # df = data.frame(hint_id = 1)
  # df$product.group = list(c(1,2))
  # This was easiest way i could think of, of keeping the hierarchical structure
  # i will leave an example here, !!!! make sure to provide the columns as lists even if there are only single elements per hint
  # according to Patrick, only fields which are multiple-select fields are implementer / product / type 
  # acting agency, hint title, hint description are immutable 
  # rest is single select. I treat 1 as T and 0 as F
  
  # input.col.names = c('hint.id','implementer.name','url','is.official','assessment.name',
  #                     'product.group.name','intervention.type.name','comment','relevance','was.modified')
  # multiple.values.permitted = c('implementer.name','product.group.name','intervention.type.name','collection.name')
  # 
  # # test setup and example of what im expecting as input
  # input = data.frame(rows=c(1,2))
  # input$hint.id = c(1,2)
  # input$implementer.name = list(c('China','Denmark'),c('Croatia')) # can a hint have multiple implementers?
  # input$url = c('sourceforhint1.com','sourceforhint2.com')
  # input$is.official = c(1,0) #don't use t/f thanks
  # input$collection.name = list(c('collection1','collection2'),c('collection1','collection5','collection7'))
  # input$assessment.name = c('unclear','liberalising')
  # input$product.group.name = list(c('uncertain','medical consumables'),c('other','food'))
  # input$intervention.type.name = list(c('uncertain','export barrier'),c('export subsidy','import barrier','other'))
  # input$comment = c('somenewcommentrelatedtohint1','somenewcommentrelatedtohint2')
  # input$relevance = c(1,0)
  # input$rows = NULL
  # ############################ for the editor i also want this variable: 
  # input$was.modified = 1 # 1 if was modified from what freelancer gave
  # 0 if not modified. I use this for processing speed / to attribute a classification_id to editor only if at least one hint was modified
  
  # if someone knows how to pass the column names as string into the ... of tidyr::unnest with multiple.values.permitted[1], be my guest
  # instead i manually pasted the names
  unnest1 = tidyr::unnest(input[,c('hint.id','implementer.name')], implementer.name) 
  unnest2 = tidyr::unnest(input[,c('hint.id','product.group.name')], product.group.name)
  unnest3 = tidyr::unnest(input[,c('hint.id','intervention.type.name')], intervention.type.name)
  unnest4 = tidyr::unnest(input[,c('hint.id','collection.name')], collection.name)
  
  processed.rows = as.data.frame(merge(merge(merge(merge(input[,input.col.names[!input.col.names %in% multiple.values.permitted]], 
                                                   unnest1, by='hint.id', all.x = T),
                                                   unnest2, by='hint.id', all.x = T),
                                                   unnest3, by='hint.id', all.x = T),
                                                   unnest4, by='hint.id', all.x = T))
                        
  temp.changes.name=paste0("b221.temp.changes.data.",user.id)
  assign(temp.changes.name,processed.rows,envir=globalenv())
  
  gta_sql_get_value(paste0("DROP TABLE IF EXISTS ",gsub('\\.','_',temp.changes.name),";"),db.connection = 'pool')
  gta_sql_create_table(write.df=temp.changes.name,
                       append.existing = F)
  
  if(is.freelancer==T){
    push.updates = paste0("/* FREELANCER UPLOAD */
                          SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log');
                          INSERT INTO bt_classification_log(classification_id, user_id, classify_option_id, time_stamp)
                          SELECT @classification_id AS classification_id, 1 AS user_id, (SELECT classify_option_id FROM bt_classify_option_list WHERE classify_option_name = 'B221') AS classify_option_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp;
                           
                          INSERT INTO b221_hint_comment_log(hint_id, user_id, comment, time_stamp)
                          SELECT DISTINCT changes.hint_id, ",user.id," AS user_id, comment, CONVERT_TZ(NOW(),'UTC','CET') AS time_stamp FROM b221_temp_changes_data_",user.id," changes;
                          
                          INSERT INTO b221_hint_assessment(hint_id, classification_id, assessment_id, assessment_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, ass_list.assessment_id, NULL AS assessment_accepted, NULL AS validation_user 
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_assessment_list ass_list ON changes.assessment_name = ass_list.assessment_name;
                          
                          INSERT INTO b221_hint_product_group(hint_id, classification_id, product_group_id, product_group_assessment, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, prod_grp_list.product_group_id, NULL AS product_group_assessment, NULL as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_product_group_list prod_grp_list ON prod_grp_list.product_group_name = changes.product_group_name;
                          
                          INSERT INTO b221_hint_intervention(hint_id, classification_id, apparent_intervention_id, intervention_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, int_type_list.intervention_type_id, NULL AS product_group_assessment, NULL as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_intervention_type_list int_type_list ON int_type_list.intervention_type_name = changes.intervention_type_name;
                          
                          # not a clue how to treat collections, are they created earlier than the b221 freelancer desk or not? can bt create them? 
                          # i assume not here, i don't check whether a hint is already part of a collection in the b221_hint_collection, i assume all b221_hint tables to be virgin, not contianing any info about the hint to be processed when at the freelancer's desk
                          INSERT INTO b221_collection_log(collection_name, user_id, last_change)
                          SELECT DISTINCT changes.collection_name, ",user.id," AS user_id, CONVERT_TZ(NOW(),'UTC','CET') AS last_change FROM b221_temp_changes_data_",user.id," changes
                          WHERE NOT EXISTS (SELECT NULL FROM b221_collection_log cltn_log WHERE cltn_log.collection_name = changes.collection_name);
                          
                          INSERT INTO b221_hint_collection(hint_id, collection_id)
                          SELECT DISTINCT changes.hint_id, cltn_log.collection_id
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_collection_log cltn_log ON cltn_log.collection_name = changes.collection_name;
                          
                          CREATE INDEX src ON b221_temp_changes_data_",user.id," (url(300));
                          
                          /* 
                          assumption here is that bastiat only pulls 1 url per hint so there will only be a single entry in bt_hint_url per hint before it is processed by b221 (not yet in the osc where people attribute more)
                          anything unofficial is classified as news by default
                          brute force approach for now, it is somewhat easily amendable to have a backlog of Bastiat's correctness (i have done it that way in the other app, but this will have to be a future project for b221)
                          INSERT INTO bt_url_log(url)
                          SELECT DISTINCT changes.url FROM b221_temp_changes_data_",user.id," changes
                          WHERE NOT EXISTS (SELECT NULL FROM bt_url_log WHERE bt_url_log.url = changes.url);
                          another assumption is that the url is immutable, only its type, so don't need to insert it and look into bt_url_log to check if it is there, it must be since it was retrieved from there
                          */
                          
                          DELETE bt_hint_url, bt_hint_jurisdiction, bt_hint_relevance FROM b221_temp_changes_data_",user.id," changes
                          JOIN bt_hint_url ON changes.hint_id = bt_hint_url.hint_id
                          LEFT JOIN bt_hint_jurisdiction ON changes.hint_id = bt_hint_jurisdiction.hint_id
                          LEFT JOIN bt_hint_relevance ON changes.hint_id = bt_hint_relevance.hint_id
                          WHERE 1 = 1;
                          
                          INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, jur_list.jurisdiction_id, NULL as jurisdiction_accepted, NULL as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_name = changes.implementer_name;
                          
                          INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, bt_url_log.url_id, (CASE WHEN changes.is_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id,
                          @classification_id AS classification_id, NULL AS url_accepted, NULL as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN bt_url_log ON bt_url_log.url = changes.url;
                          
                          INSERT INTO bt_hint_relevance(hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, changes.relevance, NULL as relevance_probability, NULL as relevance_accepted, NULL as validation_user 
                          FROM b221_temp_changes_data_",user.id," changes;
                          
                          UPDATE bt_hint_log
                          JOIN (SELECT DISTINCT(b221_temp_changes_data_",user.id,".hint_id) FROM b221_temp_changes_data_",user.id,") changes ON changes.hint_id = bt_hint_log.hint_id
                          SET bt_hint_log.hint_state_id = (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - editor desk');
  
                          DELETE bt_hint_processing FROM bt_hint_processing JOIN b221_temp_changes_data_",user.id," processed ON processed.hint_id = bt_hint_processing.hint_id WHERE 1 = 1;")
  } else {
    push.updates = paste0("/* EDITOR UPLOAD */
                          /* give editor a classification_id only if they actually modified something that they put forward to process */
                          SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log');
                          INSERT INTO bt_classification_log(classification_id, user_id, classify_option_id, time_stamp)
                          SELECT DISTINCT @classification_id AS classification_id, ",user.id," AS user_id, (SELECT classify_option_id FROM bt_classify_option_list WHERE classify_option_name = 'B221') AS classify_option_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp FROM (SELECT NULL FROM b221_temp_changes_data_",user.id," changes WHERE changes.was_modified = 1) editor_search_id;
                           
                          INSERT INTO b221_hint_comment_log(hint_id, user_id, comment, time_stamp)
                          SELECT DISTINCT changes.hint_id, ",user.id," AS user_id, comment, CONVERT_TZ(NOW(),'UTC','CET') AS time_stamp FROM b221_temp_changes_data_",user.id," changes;
                          
                          /* case where new assessment from editor */
                          INSERT INTO b221_hint_assessment(hint_id, classification_id, assessment_id, assessment_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, ass_list.assessment_id , 1 AS assessment_accepted, ",user.id," AS validation_user 
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_assessment_list ass_list ON changes.assessment_name = ass_list.assessment_name
                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_assessment ht_ass WHERE ht_ass.hint_id = changes.hint_id AND ht_ass.assessment_id = ass_list.assessment_id);
                          
                          /* validate / refuse assessment from freelancer */
                          UPDATE b221_hint_assessment ht_ass
                          JOIN b221_temp_changes_data_",user.id," changed_hints ON ht_ass.hint_id = changed_hints.hint_id
                          JOIN b221_assessment_list ass_list ON ht_ass.hint_id = ass_list.assessment_id
                          LEFT JOIN b221_temp_changes_data_",user.id," changes ON ht_ass.hint_id = changes.hint_id AND changes.assessment_name = ass_list.assessment_name
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
                          JOIN b221_temp_changes_data_",user.id," changed_hints ON prod_grp.hint_id = changed_hints.hint_id
                          JOIN b221_product_group_list prod_grp_list ON prod_grp_list.product_group_name = prod_grp.product_group_id
                          LEFT JOIN b221_temp_changes_data_",user.id," changes ON changes.hint_id = prod_grp.hint_id AND prod_grp.product_group_id = prod_grp.product_group_id
                          SET prod_grp.validation_user = ",user.id,",
                          	prod_grp.product_group_assessment = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          /* case where new intervention type from editor */
                          INSERT INTO b221_hint_intervention(hint_id, classification_id, apparent_intervention_id, intervention_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, int_type_list.intervention_type_id, 1 AS product_group_assessment, ",user.id," as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_intervention_type_list int_type_list ON int_type_list.intervention_type_name = changes.intervention_type_name
                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_intervention ht_int WHERE ht_int.hint_id = changes.hint_id AND int_type_list.intervention_type_id = ht_int.apparent_intervention_id)
                          
                          /* validate / refuse intervention type from freelancer */
                          UPDATE b221_hint_intervention ht_int 
                          JOIN b221_temp_changes_data_",user.id," changed_hints ON ht_int.hint_id = changed_hints.hint_id
                          JOIN b221_intervention_type_list int_type_list ON int_type_list.intervention_type_id = ht_int.apparent_intervention_id
                          LEFT JOIN b221_temp_changes_data_",user.id," changes ON ht_int.hint_id = changes.hint_id AND changes.intervention_type_name = int_type_list.intervention_type_name
                          SET ht_int.validation_user = ",user.id,", 
                          	ht_int.intervention_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          
                          /* INSERT NEW Collections and then add the new links */
                          INSERT INTO b221_collection_log(collection_name, user_id, last_change)
                          SELECT DISTINCT changes.collection_name, ",user.id," AS user_id, CONVERT_TZ(NOW(),'UTC','CET') AS last_change FROM b221_temp_changes_data_",user.id," changes
                          WHERE NOT EXISTS (SELECT NULL FROM b221_collection_log cltn_log WHERE cltn_log.collection_name = changes.collection_name);
                          
                          /* brute force delete all links with these hints and then reattach new ones (doesn't seem like we care who made the collection hint_id relation in this table anyways) */
                          DELETE b221_hint_collection
                          FROM b221_hint_collection 
                          JOIN b221_temp_changes_data_",user.id," changes ON changes.hint_id = b221_hint_collection.hint_id
                          WHERE 1 = 1;
                          
                          INSERT INTO b221_hint_collection(hint_id, collection_id)
                          SELECT DISTINCT changes.hint_id, cltn_log.collection_id
                          FROM b221_temp_changes_data_",user.id," changes
                          JOIN b221_collection_log cltn_log ON cltn_log.collection_name = changes.collection_name;
                          
                          CREATE INDEX src ON b221_temp_changes_data_",user.id," (url(300));
                          /* assumption here is that there is only 1 url which is pulled which is also immutable, i just need to go say yes/no to the existing entry proposed by my freelancer 
                          editor can only choose a different type 
                          */
                          INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_user)
                          SELECT changes_w_url_type.hint_id, bt_url_log.url_id, changes_w_url_type.url_type_id, @classification_id AS classification_id, 1 AS url_accepted, ",user.id," AS validation_user FROM 
                          (SELECT changes.hint_id, changes.url, (CASE WHEN changes.is_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id
                          FROM b221_temp_changes_data_",user.id," changes) changes_w_url_type
                          JOIN bt_url_log ON changes_w_url_type.url = bt_url_log.url
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_url ht_url WHERE ht_url.hint_id = changes_w_url_type.hint_id AND ht_url.url_id = bt_url_log.url_id AND ht_url.url_type_id = changes_w_url_type.url_type_id);
                          
                          UPDATE FROM bt_hint_url ht_url 
                          JOIN b221_temp_changes_data_",user.id," changed_hints ON ht_url.hint_id = changed_hints.hint_id
                          JOIN bt_url_log ON bt_url_log.url_id = ht_url.url_id
                          LEFT JOIN (SELECT changes.hint_id, changes.url, (CASE WHEN changes.is_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id
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
                          JOIN b221_temp_changes_data_",user.id," changed_hints ON ht_jur.hint_id = changed_hints.hint_id
                          JOIN gta_jurisdiction_list jur_list ON ht_jur.jurisdiction_id = jur_list.jurisdiction_id
                          LEFT JOIN b221_temp_changes_data_",user.id," changes ON ht_jur.hint_id = changes.hint_id AND changes.implementer_name = jur_list.jurisdiction_name
                          SET ht_jur.validation_user = ",user.id,", 
                          	ht_jur.jurisdiction_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                           
                          /* AND AGAIN WITH BT_HINT_RELEVANCE */
                          INSERT INTO bt_hint_relevance(hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_user)
                          SELECT DISTINCT changes.hint_id, @classification_id AS classification_id, changes.relevance, NULL as relevance_probability, 1 AS relevance_accepted, ",user.id," as validation_user
                          FROM b221_temp_changes_data_",user.id," changes
                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_relevance ht_rel WHERE ht_rel.hint_id = changes.hint_id AND ht_rel.relevance = changes.relevance);
                          
                          UPDATE bt_hint_relevance ht_rel
                          JOIN b221_temp_changes_data_",user.id," changed_hints ON ht_rel.hint_id = changed_hints.hint_id
                          LEFT JOIN b221_temp_changes_data_",user.id," changes ON ht_rel.hint_id = changes.hint_id AND changes.relevance = ht_rel.relevance
                          SET ht_rel.validation_user = ",user.id,", 
                          	ht_rel.relevance_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                          	
                          UPDATE bt_hint_log
                          JOIN (SELECT DISTINCT(b221_temp_changes_data_",user.id,".hint_id) FROM b221_temp_changes_data_",user.id,") changes ON changes.hint_id = bt_hint_log.hint_id
                          SET bt_hint_log.hint_state_id = (CASE WHEN changes.is_official = 0 THEN (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'OSC - freelancer desk') 
                                								  WHEN changes.was.is_official = 1 THEN (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'BT - ready for dispatch') END);
                            
                          DELETE bt_hint_processing FROM bt_hint_processing JOIN b221_temp_changes_data_",user.id," processed ON processed.hint_id = bt_hint_processing.hint_id WHERE 1 = 1;")
  }
  
  gta_sql_multiple_queries(push.updates, output.queries = 1, show.time = T, db.connection = db.connection)
  
  
}