b221_process_collections_hints=function(is.freelancer = NULL, user.id = NULL, new.collection.name = NULL, collection.id = NULL, hints.id = NULL, country = NULL,
                                        product = NULL, intervention = NULL, assessment = NULL, relevance = NULL, collection.unchanged = NULL){
  
  if(is.null(is.freelancer) | length(is.freelancer)!= 1 | !is.logical(is.freelancer) | is.na(is.freelancer)) stop('is.freelancer must be false if you are an editor, or true if you are a freelancer, no other value permitted')
  if(is.null(hints.id) | length(hints.id)< 1) stop('hints.id must be numeric or NA and at least length 1, expected is a vector')
  if(!xor(is.null(new.collection.name), is.null(collection.id))) stop('either collection.id or new.collection.name must be a provided, not both, not neither')
  if(!is.numeric(collection.id) & !is.character(new.collection.name)) stop('collection.id or new.collection.name must be numeric or character respectively (only one can be provided)')
  
  hints.id = unique(hints.id)
  
  # i expect vectors for: country / product / intervention 
  # single values: is.freelancer(T/F not 1/0) / user.id / relevance(1/0) / assessment / user.id 
  # i want everything in ids already, use plyr::mapvalues with lines 28-32 of server.R to convert to ids 
  
  # first update collection related information: 
  # temp values to test 
  # is.freelancer = T ; user.id = 1 ; collection.id = 1 ; country = c(1,2) ; product = c(1,2) ; intervention = c(1,2) ; assessment = 1 ; relevance = 1

  if(collection.unchanged==F){
    if(is.null(new.collection.name)){
      
      val.cltn.int = paste0("(",collection.id,",",intervention,")", collapse = ',')
      val.cltn.prod = paste0("(",collection.id,",",product,")", collapse = ',')
      val.cltn.rel = paste0("(",collection.id,",",relevance,")")[1]
      val.cltn.cty = paste0("(",collection.id,",",country,")", collapse = ',')
      val.cltn.ass = paste0("(",collection.id,",",assessment,")")[1]
      
      update.collection.info = paste0("DELETE b221_collection_intervention, b221_collection_product_group, b221_collection_relevance, b221_collection_jurisdiction, b221_collection_assessment 
                                        FROM (SELECT * FROM b221_collection_log WHERE b221_collection_log.collection_id = ",collection.id,") cltn_log
                                        LEFT JOIN b221_collection_intervention ON cltn_log.collection_id = b221_collection_intervention.collection_id
                                        LEFT JOIN b221_collection_product_group ON cltn_log.collection_id = b221_collection_product_group.collection_id
                                        LEFT JOIN b221_collection_relevance ON cltn_log.collection_id = b221_collection_relevance.collection_id
                                        LEFT JOIN b221_collection_jurisdiction ON cltn_log.collection_id = b221_collection_jurisdiction.collection_id
                                        LEFT JOIN b221_collection_assessment ON cltn_log.collection_id = b221_collection_assessment.collection_id
                                        WHERE 1 = 1;
                                        
                                        UPDATE b221_collection_log 
                                        SET b221_collection_log.user_id = ",user.id,", b221_collection_log.last_change = CONVERT_TZ(NOW(), 'UTC', 'CET') 
                                        WHERE collection_id = ",collection.id,";
                                        
                                        INSERT INTO b221_collection_intervention VALUES ",val.cltn.int,";
                                        INSERT INTO b221_collection_product_group VALUES ",val.cltn.prod,";
                                        INSERT INTO b221_collection_relevance VALUES ",val.cltn.rel,";
                                        INSERT INTO b221_collection_jurisdiction VALUES ",val.cltn.cty,";
                                        INSERT INTO b221_collection_assessment VALUES ",val.cltn.ass,";
                                        ")
      
    } else {
      
      if(length(gta_sql_get_value(paste0("SELECT collection_id FROM b221_collection_log WHERE collection_name = '",new.collection.name,"'")))>0) return('New collection was not saved! Another collection already has this name')
      
      
      new.collection <<- data.frame(collection.name = new.collection.name, user.id = user.id, last.change = substr(as.POSIXct(format(Sys.time()),tz="CET"),1,19))
      collection.id = gta_sql_append_table(table.prefix = '', append.table = 'b221.collection.log', append.by.df = "new.collection", get.id = "collection.id", db.connection = "pool")
      
      val.cltn.int = paste0("(",collection.id,",",intervention,")", collapse = ',')
      val.cltn.prod = paste0("(",collection.id,",",product,")", collapse = ',')
      val.cltn.rel = paste0("(",collection.id,",",relevance,")")[1]
      val.cltn.cty = paste0("(",collection.id,",",country,")", collapse = ',')
      val.cltn.ass = paste0("(",collection.id,",",assessment,")")[1]
      
      update.collection.info = paste0("INSERT INTO b221_collection_intervention VALUES ",val.cltn.int,";
                                       INSERT INTO b221_collection_product_group VALUES ",val.cltn.prod,";
                                       INSERT INTO b221_collection_relevance VALUES ",val.cltn.rel,";
                                       INSERT INTO b221_collection_jurisdiction VALUES ",val.cltn.cty,";
                                       INSERT INTO b221_collection_assessment VALUES ",val.cltn.ass,";
                                        ")
      
    }
    gta_sql_multiple_queries(update.collection.info, output.queries = 1)
  }
  
  original.hints = gta_sql_get_value(paste0("SELECT hint_id FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,";"))
  # states which need to be confirmed for editor-side
  state.2or8.hints = gta_sql_get_value(paste0("SELECT b221_hint_collection.hint_id FROM b221_hint_collection JOIN bt_hint_log ON b221_hint_collection.collection_id = ",collection.id," AND (bt_hint_log.hint_state_id = 2 OR bt_hint_log.hint_state_id = 8) AND bt_hint_log.hint_id = b221_hint_collection.hint_id;"))
  

  
  if(!any(is.na(hints.id))){
    # state for hints to be converted to when collection updates
    states = gta_sql_get_value(paste0("SELECT DISTINCT * FROM 
                                    (SELECT hint_state_id FROM b221_hint_collection JOIN bt_hint_log ON b221_hint_collection.collection_id = ",collection.id," AND b221_hint_collection.hint_id = bt_hint_log.hint_id
                                    UNION 
                                    SELECT hint_state_id FROM bt_hint_log WHERE hint_id IN (",paste0(hints.id, collapse=','),")) states"))
    if(relevance == 0){
      editor.new.state = 9
    } else {
      editor.new.state = ifelse(max(states)==2, 3, max(states[!states %in% 8:9]))
      if(length(editor.new.state)==0 | editor.new.state == -Inf | is.na(editor.new.state)) editor.new.state = 3
    }
    
    gta_sql_get_value(sprintf(paste0("DELETE FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id," OR b221_hint_collection.hint_id IN (%s);"),paste(hints.id, collapse = ',')))
    gta_sql_get_value(paste0("INSERT INTO b221_hint_collection VALUES ",paste0("(",hints.id,",",collection.id,")", collapse = ',')))
  } else {
    gta_sql_get_value(paste0("DELETE FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,";"))
  }
  
  # 2 cases: cascading effect is neccessary if there is a change to the collection & it's not a new collection of course! (if freelancer, then send all hints back to state 2/8, can only change collection info when it has max state 2)
  # 2nd case: reassign new hint's collection + values
  # original.hints = 1:3
  # hints.id = 1:3
  if(is.freelancer == T){
    new.hints = unique(hints.id[!hints.id %in% original.hints])
    
    select.statement.new.hints = paste0("SELECT ",new.hints[1]," AS hint_id")
    if(length(new.hints[-1])>0) select.statement.new.hints = paste0(select.statement.new.hints, ' UNION SELECT ' , paste0(new.hints[-1], collapse = ' UNION SELECT '))
    if(collection.unchanged==F & is.null(new.collection.name)){
      
      update.collection.hints  = paste0(" SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log');
                                          INSERT INTO bt_classification_log(classification_id, user_id, hint_state_id, time_stamp)
                                          SELECT @classification_id AS classification_id, ",user.id," AS user_id, (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - freelancer desk') AS hint_state_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp;
                                           
                                          DELETE b221_hint_assessment, b221_hint_product_group, b221_hint_intervention 
                                          FROM (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") ht_cltn
                                          LEFT JOIN b221_hint_assessment ON ht_cltn.hint_id = b221_hint_assessment.hint_id
                                          LEFT JOIN b221_hint_product_group ON ht_cltn.hint_id = b221_hint_product_group.hint_id
                                          LEFT JOIN b221_hint_intervention ON ht_cltn.hint_id = b221_hint_intervention.hint_id
                                          WHERE 1 = 1;
                                          
                                          INSERT INTO b221_hint_assessment(hint_id, classification_id, assessment_id, assessment_accepted, validation_user)
                                          SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_ass.assessment_id, NULL AS assessment_accepted, NULL AS validation_user 
                                          FROM (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") ht_cltn 
                                          JOIN b221_collection_assessment cltn_ass ON ht_cltn.collection_id = cltn_ass.collection_id;
                                          
                                          INSERT INTO b221_hint_product_group(hint_id, classification_id, product_group_id, product_group_assessment, validation_user)
                                          SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_prod.product_group_id, NULL AS product_group_assessment, NULL AS validation_user 
                                          FROM (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") ht_cltn 
                                          JOIN b221_collection_product_group cltn_prod ON ht_cltn.collection_id = cltn_prod.collection_id;
                                          
                                          INSERT INTO b221_hint_intervention(hint_id, classification_id, apparent_intervention_id, intervention_accepted, validation_user)
                                          SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_int.intervention_type_id, NULL AS intervention_accepted, NULL AS validation_user 
                                          FROM (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") ht_cltn 
                                          JOIN b221_collection_intervention cltn_int ON ht_cltn.collection_id = cltn_int.collection_id;
                                            
                                          DELETE bt_hint_jurisdiction, bt_hint_relevance 
                                          FROM (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") ht_cltn
                                          LEFT JOIN bt_hint_jurisdiction ON ht_cltn.hint_id = bt_hint_jurisdiction.hint_id
                                          LEFT JOIN bt_hint_relevance ON ht_cltn.hint_id = bt_hint_relevance.hint_id
                                          WHERE 1 = 1;
                                          
                                          INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_user)
                                          SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_jur.jurisdiction_id, NULL AS jurisdiction_accepted, NULL AS validation_user 
                                          FROM (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") ht_cltn 
                                          JOIN b221_collection_jurisdiction cltn_jur ON ht_cltn.collection_id = cltn_jur.collection_id;
                                         
                                          INSERT INTO bt_hint_relevance(hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_user)
                                          SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_rel.relevance, NULL as relevance_probability, NULL AS relevance_accepted, NULL AS validation_user 
                                          FROM (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") ht_cltn 
                                          JOIN b221_collection_relevance cltn_rel ON ht_cltn.collection_id = cltn_rel.collection_id;
                                        
                                          UPDATE bt_hint_log
                                          JOIN (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") ht_cltn ON ht_cltn.hint_id = bt_hint_log.hint_id
                                          JOIN b221_collection_relevance cltn_rel ON ht_cltn.collection_id = cltn_rel.collection_id
                                          SET bt_hint_log.hint_state_id = (CASE WHEN cltn_rel.relevance = 1 THEN (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - editor desk') ELSE 
                                                              (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'trash bin - entered') END);")
      gta_sql_multiple_queries(update.collection.hints, output.queries = 1)
      
    
  } else {
    
    if(!any(is.na(new.hints)) & length(new.hints) != 0){
    update.collection.hints  = paste0(" SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log');
                                        INSERT INTO bt_classification_log(classification_id, user_id, hint_state_id, time_stamp)
                                        SELECT @classification_id AS classification_id, ",user.id," AS user_id, (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - freelancer desk') AS hint_state_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp;
                                         
                                        DELETE b221_hint_assessment, b221_hint_product_group, b221_hint_intervention 
                                        FROM (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) ht_cltn
                                        LEFT JOIN b221_hint_assessment ON ht_cltn.hint_id = b221_hint_assessment.hint_id
                                        LEFT JOIN b221_hint_product_group ON ht_cltn.hint_id = b221_hint_product_group.hint_id
                                        LEFT JOIN b221_hint_intervention ON ht_cltn.hint_id = b221_hint_intervention.hint_id
                                        WHERE 1 = 1;
                                        
                                        INSERT INTO b221_hint_assessment(hint_id, classification_id, assessment_id, assessment_accepted, validation_user)
                                        SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_ass.assessment_id, NULL AS assessment_accepted, NULL AS validation_user 
                                        FROM (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) ht_cltn 
                                        JOIN b221_collection_assessment cltn_ass ON ht_cltn.collection_id = cltn_ass.collection_id;
                                        
                                        INSERT INTO b221_hint_product_group(hint_id, classification_id, product_group_id, product_group_assessment, validation_user)
                                        SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_prod.product_group_id, NULL AS product_group_assessment, NULL AS validation_user 
                                        FROM (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) ht_cltn 
                                        JOIN b221_collection_product_group cltn_prod ON ht_cltn.collection_id = cltn_prod.collection_id;
                                        
                                        INSERT INTO b221_hint_intervention(hint_id, classification_id, apparent_intervention_id, intervention_accepted, validation_user)
                                        SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_int.intervention_type_id, NULL AS intervention_accepted, NULL AS validation_user 
                                        FROM (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) ht_cltn 
                                        JOIN b221_collection_intervention cltn_int ON ht_cltn.collection_id = cltn_int.collection_id;
                                          
                                        DELETE bt_hint_jurisdiction, bt_hint_relevance 
                                        FROM (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) ht_cltn
                                        LEFT JOIN bt_hint_jurisdiction ON ht_cltn.hint_id = bt_hint_jurisdiction.hint_id
                                        LEFT JOIN bt_hint_relevance ON ht_cltn.hint_id = bt_hint_relevance.hint_id
                                        WHERE 1 = 1;
                                        
                                        INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_user)
                                        SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_jur.jurisdiction_id, NULL AS jurisdiction_accepted, NULL AS validation_user 
                                        FROM (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) ht_cltn 
                                        JOIN b221_collection_jurisdiction cltn_jur ON ht_cltn.collection_id = cltn_jur.collection_id;
                                       
                                        INSERT INTO bt_hint_relevance(hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_user)
                                        SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_rel.relevance, NULL as relevance_probability, NULL AS relevance_accepted, NULL AS validation_user 
                                        FROM (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) ht_cltn 
                                        JOIN b221_collection_relevance cltn_rel ON ht_cltn.collection_id = cltn_rel.collection_id;
                                      
                                        UPDATE bt_hint_log
                                        JOIN (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) ht_cltn ON ht_cltn.hint_id = bt_hint_log.hint_id
                                        JOIN b221_collection_relevance cltn_rel ON ht_cltn.collection_id = cltn_rel.collection_id
                                        SET bt_hint_log.hint_state_id = (CASE WHEN cltn_rel.relevance = 1 THEN (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - editor desk') ELSE 
                                                            (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'trash bin - entered') END);")
    
    gta_sql_multiple_queries(update.collection.hints, output.queries = 1)
    
      }
    }
  } else {
    # editor side 
    
    # on the editor side even if the collection already contains the hint but it was submitted by a freelancer, a confirmation needs to occur
    if(!is.na(state.2or8.hints)) new.hints = unique(c(state.2or8.hints,hints.id[!hints.id %in% original.hints])) else new.hints = unique(hints.id[!hints.id %in% original.hints])
    
    select.statement.new.hints = paste0("SELECT ",new.hints[1]," AS hint_id")
    if(length(new.hints[-1])>0) select.statement.new.hints = paste0(select.statement.new.hints, ' UNION SELECT ' , paste0(new.hints[-1], collapse = ' UNION SELECT '))

    if(collection.unchanged==F & is.null(new.collection.name)){
    update.collection.hints  = paste0("SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log');
                                          INSERT INTO bt_classification_log(classification_id, user_id, hint_state_id, time_stamp)
                                          SELECT DISTINCT @classification_id AS classification_id, ",user.id," AS user_id, (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - editor desk') AS hint_state_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp;
                                           
                                          INSERT INTO b221_hint_assessment(hint_id, classification_id, assessment_id, assessment_accepted, validation_user)
                                          SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_ass.assessment_id , 1 AS assessment_accepted, ",user.id," AS validation_user 
                                          FROM (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") ht_cltn
                                          JOIN b221_collection_assessment cltn_ass ON ht_cltn.collection_id = cltn_ass.collection_id
                                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_assessment ht_ass WHERE ht_ass.hint_id = ht_cltn.hint_id AND ht_ass.assessment_id = cltn_ass.assessment_id);
                                          
                                          UPDATE b221_hint_assessment ht_ass
                                          JOIN (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") changed_hints ON ht_ass.hint_id = changed_hints.hint_id
                                          LEFT JOIN (SELECT b221_hint_collection.hint_id, b221_collection_assessment.assessment_id FROM b221_hint_collection JOIN b221_collection_assessment ON b221_hint_collection.collection_id = ",collection.id," AND b221_hint_collection.collection_id = b221_collection_assessment.collection_id) changes 
                                          ON ht_ass.hint_id = changes.hint_id AND ht_ass.assessment_id = changes.assessment_id
                                          SET ht_ass.validation_user = ",user.id,",
                                          	ht_ass.assessment_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                                          
                                          INSERT INTO b221_hint_product_group(hint_id, classification_id, product_group_id, product_group_assessment, validation_user)
                                          SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_prod.product_group_id , 1 AS assessment_accepted, ",user.id," AS validation_user 
                                          FROM (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") ht_cltn
                                          JOIN b221_collection_product_group cltn_prod ON ht_cltn.collection_id = cltn_prod.collection_id
                                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_product_group prod_grp WHERE prod_grp.hint_id = ht_cltn.hint_id AND prod_grp.product_group_id = cltn_prod.product_group_id);
                                         
                                          UPDATE b221_hint_product_group prod_grp
                                          JOIN (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") changed_hints ON prod_grp.hint_id = changed_hints.hint_id
                                          LEFT JOIN (SELECT b221_hint_collection.hint_id, b221_collection_product_group.product_group_id FROM b221_hint_collection JOIN b221_collection_product_group ON b221_hint_collection.collection_id = ",collection.id," AND b221_hint_collection.collection_id = b221_collection_product_group.collection_id) changes 
                                          ON prod_grp.hint_id = changes.hint_id AND prod_grp.product_group_id = changes.product_group_id
                                          SET prod_grp.validation_user = ",user.id,",
                                          	prod_grp.product_group_assessment = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                                          
                                          INSERT INTO b221_hint_intervention(hint_id, classification_id, apparent_intervention_id, intervention_accepted, validation_user)
                                          SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_int.intervention_type_id , 1 AS assessment_accepted, ",user.id," AS validation_user 
                                          FROM (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") ht_cltn
                                          JOIN b221_collection_intervention cltn_int ON ht_cltn.collection_id = cltn_int.collection_id
                                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_intervention ht_int WHERE ht_int.hint_id = ht_cltn.hint_id AND cltn_int.intervention_type_id = ht_int.apparent_intervention_id);
                                          
                                          UPDATE b221_hint_intervention ht_int 
                                          JOIN (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") changed_hints ON ht_int.hint_id = changed_hints.hint_id
                                          LEFT JOIN (SELECT b221_hint_collection.hint_id, b221_collection_intervention.intervention_type_id FROM b221_hint_collection JOIN b221_collection_intervention ON b221_hint_collection.collection_id = ",collection.id," AND b221_hint_collection.collection_id = b221_collection_intervention.collection_id) changes 
                                          ON ht_int.hint_id = changes.hint_id AND ht_int.apparent_intervention_id = changes.intervention_type_id
                                          SET ht_int.validation_user = ",user.id,", 
                                          	ht_int.intervention_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                                          
                                          INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_user)
                                          SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_jur.jurisdiction_id, 1 AS assessment_accepted, ",user.id," AS validation_user 
                                          FROM (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") ht_cltn
                                          JOIN b221_collection_jurisdiction cltn_jur ON ht_cltn.collection_id = cltn_jur.collection_id
                                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_jurisdiction ht_jur WHERE ht_jur.hint_id = ht_cltn.hint_id AND cltn_jur.jurisdiction_id = ht_jur.jurisdiction_id);
                                           
                                          UPDATE bt_hint_jurisdiction ht_jur 
                                          JOIN (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") changed_hints ON ht_jur.hint_id = changed_hints.hint_id
                                          LEFT JOIN (SELECT b221_hint_collection.hint_id, b221_collection_jurisdiction.jurisdiction_id FROM b221_hint_collection JOIN b221_collection_jurisdiction ON b221_hint_collection.collection_id = ",collection.id," AND b221_hint_collection.collection_id = b221_collection_jurisdiction.collection_id) changes 
                                          ON ht_jur.hint_id = changes.hint_id AND ht_jur.jurisdiction_id = changes.jurisdiction_id
                                          SET ht_jur.validation_user = ",user.id,", 
                                          	ht_jur.jurisdiction_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                                          
                                          INSERT INTO bt_hint_relevance(hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_user)
                                          SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_rel.relevance, NULL as relevance_probability, 1 AS relevance_accepted, ",user.id," AS validation_user 
                                          FROM (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") ht_cltn
                                          JOIN b221_collection_relevance cltn_rel ON ht_cltn.collection_id = cltn_rel.collection_id
                                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_relevance ht_rel WHERE ht_rel.hint_id = ht_cltn.hint_id AND cltn_rel.relevance = ht_rel.relevance);
                                          
                                          UPDATE bt_hint_relevance ht_rel
                                          JOIN (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collection.id,") changed_hints ON ht_rel.hint_id = changed_hints.hint_id
                                          LEFT JOIN (SELECT b221_hint_collection.hint_id, b221_collection_relevance.relevance FROM b221_hint_collection JOIN b221_collection_relevance ON b221_hint_collection.collection_id = ",collection.id," AND b221_hint_collection.collection_id = b221_collection_relevance.collection_id) changes 
                                          ON ht_rel.hint_id = changes.hint_id AND ht_rel.relevance = changes.relevance
                                          SET ht_rel.validation_user = ",user.id,", 
                                          	ht_rel.relevance_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                                      
                                          UPDATE bt_hint_log
                                          JOIN b221_hint_collection ON b221_hint_collection.collection_id = ",collection.id," AND b221_hint_collection.hint_id = bt_hint_log.hint_id
                                          SET bt_hint_log.hint_state_id = ",editor.new.state,";")
    
    gta_sql_multiple_queries(update.collection.hints, output.queries = 1)
    } else {
      
      if(!any(is.na(new.hints)) & length(new.hints) != 0){
      #editor reassigned hints
      update.collection.hints  = paste0("SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log');
                                          INSERT INTO bt_classification_log(classification_id, user_id, hint_state_id, time_stamp)
                                          SELECT DISTINCT @classification_id AS classification_id, ",user.id," AS user_id, (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - editor desk') AS hint_state_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp;
                                           
                                          INSERT INTO b221_hint_assessment(hint_id, classification_id, assessment_id, assessment_accepted, validation_user)
                                          SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_ass.assessment_id , 1 AS assessment_accepted, ",user.id," AS validation_user 
                                          FROM (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) ht_cltn
                                          JOIN b221_collection_assessment cltn_ass ON ht_cltn.collection_id = cltn_ass.collection_id
                                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_assessment ht_ass WHERE ht_ass.hint_id = ht_cltn.hint_id AND ht_ass.assessment_id = cltn_ass.assessment_id);
                                          
                                          UPDATE b221_hint_assessment ht_ass
                                          JOIN (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) changed_hints ON ht_ass.hint_id = changed_hints.hint_id
                                          LEFT JOIN (SELECT b221_hint_collection.hint_id, b221_collection_assessment.assessment_id FROM b221_hint_collection JOIN b221_collection_assessment ON b221_hint_collection.collection_id = ",collection.id," AND b221_hint_collection.collection_id = b221_collection_assessment.collection_id) changes 
                                          ON ht_ass.hint_id = changes.hint_id AND ht_ass.assessment_id = changes.assessment_id
                                          SET ht_ass.validation_user = ",user.id,",
                                          	ht_ass.assessment_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                                          
                                          INSERT INTO b221_hint_product_group(hint_id, classification_id, product_group_id, product_group_assessment, validation_user)
                                          SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_prod.product_group_id , 1 AS assessment_accepted, ",user.id," AS validation_user 
                                          FROM (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) ht_cltn
                                          JOIN b221_collection_product_group cltn_prod ON ht_cltn.collection_id = cltn_prod.collection_id
                                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_product_group prod_grp WHERE prod_grp.hint_id = ht_cltn.hint_id AND prod_grp.product_group_id = cltn_prod.product_group_id);
                                         
                                          UPDATE b221_hint_product_group prod_grp
                                          JOIN (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) changed_hints ON prod_grp.hint_id = changed_hints.hint_id
                                          LEFT JOIN (SELECT b221_hint_collection.hint_id, b221_collection_product_group.product_group_id FROM b221_hint_collection JOIN b221_collection_product_group ON b221_hint_collection.collection_id = ",collection.id," AND b221_hint_collection.collection_id = b221_collection_product_group.collection_id) changes 
                                          ON prod_grp.hint_id = changes.hint_id AND prod_grp.product_group_id = changes.product_group_id
                                          SET prod_grp.validation_user = ",user.id,",
                                          	prod_grp.product_group_assessment = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                                          
                                          INSERT INTO b221_hint_intervention(hint_id, classification_id, apparent_intervention_id, intervention_accepted, validation_user)
                                          SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_int.intervention_type_id , 1 AS assessment_accepted, ",user.id," AS validation_user 
                                          FROM (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) ht_cltn
                                          JOIN b221_collection_intervention cltn_int ON ht_cltn.collection_id = cltn_int.collection_id
                                          WHERE NOT EXISTS (SELECT NULL FROM b221_hint_intervention ht_int WHERE ht_int.hint_id = ht_cltn.hint_id AND cltn_int.intervention_type_id = ht_int.apparent_intervention_id);
                                          
                                          UPDATE b221_hint_intervention ht_int 
                                          JOIN (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) changed_hints ON ht_int.hint_id = changed_hints.hint_id
                                          LEFT JOIN (SELECT b221_hint_collection.hint_id, b221_collection_intervention.intervention_type_id FROM b221_hint_collection JOIN b221_collection_intervention ON b221_hint_collection.collection_id = ",collection.id," AND b221_hint_collection.collection_id = b221_collection_intervention.collection_id) changes 
                                          ON ht_int.hint_id = changes.hint_id AND ht_int.apparent_intervention_id = changes.intervention_type_id
                                          SET ht_int.validation_user = ",user.id,", 
                                          	ht_int.intervention_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                                          
                                          INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_user)
                                          SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_jur.jurisdiction_id, 1 AS assessment_accepted, ",user.id," AS validation_user 
                                          FROM (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) ht_cltn
                                          JOIN b221_collection_jurisdiction cltn_jur ON ht_cltn.collection_id = cltn_jur.collection_id
                                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_jurisdiction ht_jur WHERE ht_jur.hint_id = ht_cltn.hint_id AND cltn_jur.jurisdiction_id = ht_jur.jurisdiction_id);
                                           
                                          UPDATE bt_hint_jurisdiction ht_jur 
                                          JOIN (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) changed_hints ON ht_jur.hint_id = changed_hints.hint_id
                                          LEFT JOIN (SELECT b221_hint_collection.hint_id, b221_collection_jurisdiction.jurisdiction_id FROM b221_hint_collection JOIN b221_collection_jurisdiction ON b221_hint_collection.collection_id = ",collection.id," AND b221_hint_collection.collection_id = b221_collection_jurisdiction.collection_id) changes 
                                          ON ht_jur.hint_id = changes.hint_id AND ht_jur.jurisdiction_id = changes.jurisdiction_id
                                          SET ht_jur.validation_user = ",user.id,", 
                                          	ht_jur.jurisdiction_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                                          
                                          INSERT INTO bt_hint_relevance(hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_user)
                                          SELECT DISTINCT ht_cltn.hint_id, @classification_id AS classification_id, cltn_rel.relevance, NULL as relevance_probability, 1 AS relevance_accepted, ",user.id," AS validation_user 
                                          FROM (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) ht_cltn
                                          JOIN b221_collection_relevance cltn_rel ON ht_cltn.collection_id = cltn_rel.collection_id
                                          WHERE NOT EXISTS (SELECT NULL FROM bt_hint_relevance ht_rel WHERE ht_rel.hint_id = ht_cltn.hint_id AND cltn_rel.relevance = ht_rel.relevance);
                                          
                                          UPDATE bt_hint_relevance ht_rel
                                          JOIN (SELECT reassigned_hints.hint_id, ",collection.id," AS collection_id FROM (",select.statement.new.hints,") reassigned_hints) changed_hints ON ht_rel.hint_id = changed_hints.hint_id
                                          LEFT JOIN (SELECT b221_hint_collection.hint_id, b221_collection_relevance.relevance FROM b221_hint_collection JOIN b221_collection_relevance ON b221_hint_collection.collection_id = ",collection.id," AND b221_hint_collection.collection_id = b221_collection_relevance.collection_id) changes 
                                          ON ht_rel.hint_id = changes.hint_id AND ht_rel.relevance = changes.relevance
                                          SET ht_rel.validation_user = ",user.id,", 
                                          	ht_rel.relevance_accepted = (CASE WHEN changes.hint_id IS NOT NULL THEN 1 ELSE 0 END);
                                        
                                          UPDATE bt_hint_log
                                          JOIN b221_hint_collection ON b221_hint_collection.collection_id = ",collection.id," AND b221_hint_collection.hint_id = bt_hint_log.hint_id
                                          SET bt_hint_log.hint_state_id = ",editor.new.state,";")
      
      gta_sql_multiple_queries(update.collection.hints, output.queries = 1)
      
      }
      
    }
  }
  return('successful')
}
