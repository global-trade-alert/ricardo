osc_process_display_info = function(is.freelancer = NULL, user.id = NULL, processed.rows = NULL){
  
  # the app is supposed to: 
  # 1. recieve the pulled information from certain hints via osc_pull_display_info - which differentiates for editor versus freelancer
  # 2. the data is transformed from wide to long in the osc_pull_display_info, the app needs to dissassemble the " ; " separated urls / comments (sorted by descending date) to display them
  # 3. - urls are separated by ' ; ' into some boxes which the editors can drag and drop into categories (off. news, consult, other)
  # 4a. - for the freelancer: mark those rows which were processed as green, neutral as in not processed 
  #     - only use osc_process_display on those which were processed!
  #     - was.modified does not exist for the freelancer, everything that is green is considered to have been modified (i assume the freelancer is going to be adding nearly always some sort of modification, the editor not, so i don't care as much if the processing speed is lost on freelancer if he changes 90% of hints, but editor only changes 10%)
  # 4b. - for the editor: mark those rows which were processed as either green (was.accepted = 1) or red (was.accepted = 0), this determines if the hint goes back to freelancer or moves to next stage
  #     - was.accepted = NA doesn't get fed into the osc_process_display
  #     - set to each modified hint, was.modified to 1 if this hint's urls / files are changed (avoids unnecessary accessing of tables / computing), set was.modified = 0 if editor is just confirming what freelancer did but with no amendments
  # 5. reminder: this is all done with a wide dataframe with ' ; ' separating urls if there are multiple within the same type (off. , news, consult, other), the submitted dataframe is also wide and this function itself makes the df long
  # 6. "new.comment" is the column which this function picks up for new comments
  # 7. feed this into the osc_process_display_info with a submit button somewhere (not automatic)
  # 8. remove those hints which were processed from display (if the freelancer submits twice the same hint without it being sent back to him by the editor, we are in trouble, function doesn't support this)
  # 9. need to think about how i can allocate x hints to each user using the app without them getting the same hints? 
  
  # ensure two official sources have not been attributed to the same hint 
  no.dup.official = na.omit(unique(processed.rows[,c('hint.id','official')]))
  if(any(duplicated(no.dup.official$hint.id) == T)) stop('two official sources were attributed to the same hint?')
  rm('no.dup.official')
  
  processed.rows = tidyr::gather(processed.rows,key = 'url.type.name', value = 'url',official, news, consultancy, other)
  processed.rows = splitstackshape::cSplit(processed.rows, match('url.type.name',names(processed.rows)), sep = ' ; ', direction = 'long')
  processed.rows$url.type.name = stringr::str_trim(processed.rows$url.type.name)                 
  processed.rows$url.id = NA
  processed.rows$url.type.id = NA

  temp.changes.name=paste0("temp.changes.data.",user.id)
  assign(temp.changes.name,processed.rows,envir=globalenv())
  
  gta_sql_get_value(paste0("DROP TABLE IF EXISTS osc_",gsub('\\.','_',temp.changes.name),";"),db.connection = 'pool')
  gta_sql_create_table(write.df=temp.changes.name,
                       append.existing = F)
  
  if(is.freelancer==T){
    push.updates = paste0("/* FREELANCER UPDATES */
                              CREATE INDEX src ON osc_temp_changes_data_",user.id," (url(300));
                              
                              INSERT INTO bt_url_log(url)
                              SELECT DISTINCT url FROM osc_temp_changes_data_",user.id," srces
                              WHERE srces.url IS NOT NULL
                              AND NOT EXISTS
                              (SELECT NULL FROM bt_url_log WHERE bt_url_log.url = srces.url);
                              
                              UPDATE osc_temp_changes_data_",user.id,"
                              INNER JOIN bt_url_log ON osc_temp_changes_data_",user.id,".url = bt_url_log.url
                              INNER JOIN bt_url_type_list ON osc_temp_changes_data_",user.id,".url_type_name = bt_url_type_list.url_type_name
                              SET osc_temp_changes_data_",user.id,".url_id = bt_url_log.url_id, osc_temp_changes_data_",user.id,".url_type_id = bt_url_type_list.url_type_id;
                              
                              CREATE INDEX hint_src ON osc_temp_changes_data_",user.id," (hint_id, url_id, url_type_id);
                              
                              SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log');
                              INSERT INTO bt_classification_log(classification_id, user_id, classify_option_id, time_stamp)
                              SELECT @classification_id AS classification_id, ",user.id," AS user_id, (SELECT classify_option_id FROM bt_classify_option_list WHERE classify_option_name = 'OSC') AS classify_option_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp;
                              
                              INSERT INTO osc_file_log(file_path)
                              SELECT file_path FROM osc_temp_changes_data_",user.id," changes WHERE changes.was_modified = 1 AND changes.file_path IS NOT NULL AND NOT EXISTS (SELECT NULL FROM osc_file_log WHERE osc_file_log.file_path = changes.file_path);
                              
                              INSERT INTO osc_hint_file(hint_id, file_id)
                              SELECT changes.hint_id, osc_file_log.file_id FROM osc_temp_changes_data_",user.id," changes
                              JOIN osc_file_log ON changes.file_path = osc_file_log.file_path
                              AND NOT EXISTS (SELECT NULL FROM osc_hint_file WHERE changes.hint_id = osc_hint_file.hint_id AND osc_file_log.file_id = osc_hint_file.file_id);
                              
                              INSERT INTO osc_hint_comment_log(hint_id, user_id, comment, time_stamp)
                              SELECT DISTINCT(changes.hint_id), ",user.id," AS user_id, new_comment AS comment, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp FROM osc_temp_changes_data_",user.id," changes
                              WHERE changes.new_comment IS NOT NULL;
                              
                              /* insert new hint + url + url type pairs and leave url_accepted and validation_user as NULL */
                              INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_user)
                              SELECT changes.hint_id, bt_url_log.url_id, bt_url_type_list.url_type_id, @classification_id AS classification_id, NULL AS url_accepted, NULL AS validation_user
                              FROM (SELECT * FROM osc_temp_changes_data_",user.id," WHERE url_id IS NOT NULL) changes
                              WHERE NOT EXISTS
                              (SELECT NULL FROM bt_hint_url WHERE bt_hint_url.hint_id = changes.hint_id AND bt_hint_url.url_id = changes.url_id AND bt_url_type_list.url_type_id = changes.url_type_id);
                              
                              /* search_id: keep the same search_id as in the db, unless: freelancer picks up url that bastiat proposed (search_id = null), or for those which were accepted by editor but freelancer kills (disagrees)
                               * validation_user: null if url provided by freelancer was previously not accepted by editor, or if freelancer removes url that was previously added by editor (pending approval)
                               * url_accepted: 1 if editor or freelancer previously added it correctly and new upload confirms it, 0 if new upload removes a url which was previously evaluated (validation_user is then set to NULL), NULL otherwise
                               *  */ 
                              UPDATE bt_hint_url
                              JOIN osc_temp_changes_data_",user.id," changed_hints ON bt_hint_url.hint_id = changed_hints.hint_id
                              LEFT JOIN osc_temp_changes_data_",user.id," changes ON changed_hints.hint_id = changes.hint_id AND bt_hint_url.url_id = changes.url_id AND bt_hint_url.url_type_id = changes.url_type_id
                              SET bt_hint_url.classification_id = (CASE WHEN ((changes.url_id IS NOT NULL AND bt_hint_url.classification_id IS NULL) OR (changes.url_id IS NULL AND bt_hint_url.url_accepted = 1)) THEN @classification_id ELSE bt_hint_url.classification_id END),
                              	bt_hint_url.url_accepted = (CASE WHEN (changes.url_id IS NOT NULL AND bt_hint_url.url_accepted = 1) THEN 1
                              									 WHEN (changes.url_id IS NULL AND bt_hint_url.url_accepted IS NOT NULL) THEN 0 ELSE NULL END),
                              	bt_hint_url.validation_user = (CASE WHEN ((changes.url_id IS NOT NULL AND bt_hint_url.url_accepted = 0) OR (changes.url_id IS NULL AND bt_hint_url.url_accepted = 1)) THEN NULL ELSE bt_hint_url.validation_user END);
                              
                              UPDATE bt_hint_log
                              JOIN (SELECT DISTINCT(osc_temp_changes_data_",user.id,".hint_id) FROM osc_temp_changes_data_",user.id,") changes ON changes.hint_id = bt_hint_log.hint_id
                              SET bt_hint_log.hint_state_id = (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'OSC - editor desk');
                          
                              DELETE bt_hint_processing FROM bt_hint_processing JOIN osc_temp_changes_data_1 processed ON processed.hint_id = bt_hint_processing.hint_id WHERE 1 = 1;")
  } else {
    push.updates = paste0("/* EDITOR UPDATES */
                              CREATE INDEX src ON osc_temp_changes_data_",user.id," (url(300));
                              
                              INSERT INTO bt_url_log(url)
                              SELECT DISTINCT url FROM osc_temp_changes_data_",user.id," srces
                              WHERE srces.url IS NOT NULL AND srces.was_modified = 1
                              AND NOT EXISTS
                              (SELECT NULL FROM bt_url_log WHERE bt_url_log.url = srces.url);
                              
                              UPDATE osc_temp_changes_data_",user.id,"
                              INNER JOIN bt_url_log ON osc_temp_changes_data_",user.id,".url = bt_url_log.url
                              INNER JOIN bt_url_type_list ON osc_temp_changes_data_",user.id,".url_type_name = bt_url_type_list.url_type_name
                              SET osc_temp_changes_data_",user.id,".url_id = bt_url_log.url_id, osc_temp_changes_data_",user.id,".url_type_id = bt_url_type_list.url_type_id;
                              
                              CREATE INDEX hint_src ON osc_temp_changes_data_",user.id," (hint_id, url_id, url_type_id);
                              
                              /* give editor a search_id only if they added a source to an entry in one of the hints they processed, otherwise don't */
                              SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log');
                              INSERT INTO bt_classification_log(classification_id, user_id, classify_option_id, time_stamp)
                              SELECT DISTINCT @classification_id AS classification_id, ",user.id," AS user_id, (SELECT classify_option_id FROM bt_classify_option_list WHERE classify_option_name = 'OSC') AS classify_option_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp FROM (SELECT NULL FROM osc_temp_changes_data_",user.id," changes WHERE changes.was_modified = 1) editor_search_id;
                              
                              INSERT INTO osc_file_log(file_path)
                              SELECT file_path FROM osc_temp_changes_data_",user.id," changes WHERE changes.was_modified = 1 AND changes.file_path IS NOT NULL AND NOT EXISTS (SELECT NULL FROM osc_file_log WHERE osc_file_log.file_path = changes.file_path);
                              
                              INSERT INTO osc_hint_file(hint_id, file_id)
                              SELECT changes.hint_id, osc_file_log.file_id FROM osc_temp_changes_data_",user.id," changes
                              JOIN osc_file_log ON changes.file_path = osc_file_log.file_path
                              WHERE changes.was_modified = 1
                              AND NOT EXISTS (SELECT NULL FROM osc_hint_file WHERE changes.hint_id = osc_hint_file.hint_id AND osc_file_log.file_id = osc_hint_file.file_id);
                              
                              INSERT INTO osc_hint_comment_log(hint_id, user_id, comment, time_stamp)
                              SELECT DISTINCT(changes.hint_id), ",user.id," AS user_id, new_comment AS comment, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp FROM osc_temp_changes_data_",user.id," changes
                              WHERE changes.new_comment IS NOT NULL;
                              
                              /* insert new hint + url + url type pairs and update url_accepted and validation_user to 1 and editor_user_id */ 
                              INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_user)
                              SELECT changes.hint_id, changes.url_id, changes.url_type_id, @classification_id AS classification_id, 1 AS url_accepted, ",user.id," AS validation_user
                              FROM (SELECT * FROM osc_temp_changes_data_",user.id," WHERE was_modified = 1 AND url_id IS NOT NULL) changes
                              WHERE NOT EXISTS
                              (SELECT NULL FROM bt_hint_url WHERE bt_hint_url.hint_id = changes.hint_id AND bt_hint_url.url_id = changes.url_id AND bt_hint_url.url_type_id = changes.url_type_id);
                              
                              /* those urls which bastiat suggests and the freelancer did not are attributed the editor's search_id
                               * those urls which were found and the editor discarded are marked as url_accepted = 0 and validation_user is the editor's user_id
                               * when match but search id is empty -> attribute editor search_id
                               * when match but non-empty then keep same search_id
                               * when no match, leave search_id untouched */
                              UPDATE bt_hint_url
                              JOIN osc_temp_changes_data_",user.id," changed_hints ON bt_hint_url.hint_id = changed_hints.hint_id
                              LEFT JOIN osc_temp_changes_data_",user.id," changes ON changed_hints.hint_id = changes.hint_id AND bt_hint_url.url_id = changes.url_id AND bt_hint_url.url_type_id = changes.url_type_id
                              SET bt_hint_url.classification_id = (CASE WHEN (changes.url_id IS NOT NULL AND bt_hint_url.classification_id IS NULL) THEN @classification_id ELSE bt_hint_url.classification_id END),
                              	bt_hint_url.url_accepted = (CASE WHEN (changes.url_id IS NOT NULL) THEN 1 ELSE 0 END),
                              	bt_hint_url.validation_user = ",user.id,";
                              
                              /* 'OSC - freelancer desk' change state to this if was_accepted = 0 (assigned on hint level in-app) 
                               * 'lead - sent out' change state to this if was_accepted = 1 (assigned on hint level in-app)
                               * for editor hints are either accepted or refused was_accepted = 1/0, no neutral option permissible */
                              UPDATE bt_hint_log
                              JOIN (SELECT DISTINCT osc_temp_changes_data_",user.id,".hint_id, osc_temp_changes_data_",user.id,".was_accepted FROM osc_temp_changes_data_",user.id,") changes ON changes.hint_id = bt_hint_log.hint_id
                              SET bt_hint_log.hint_state_id = (CASE WHEN changes.was_accepted = 0 THEN (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'OSC - freelancer desk') 
                              									  WHEN changes.was.accepted = 1 THEN (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'BT - ready for dispatch') END);
                          
                              DELETE bt_hint_processing FROM bt_hint_processing JOIN osc_temp_changes_data_1 processed ON processed.hint_id = bt_hint_processing.hint_id WHERE 1 = 1;")
  }
  
  gta_sql_multiple_queries(push.updates, output.queries = 1, show.time = T, db.connection = db.connection)
  
}