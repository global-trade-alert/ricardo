# library(pool)
# library(gtasql)
# library(gtalibrary)
# library(tidyverse)
# library(plyr)
# gta_setwd()
#gta_sql_pool_open(db.title="ricardodev",
#                  db.host = gta_pwd("ricardodev")$host,
#                  db.name = "dlvr_app_extension_dev",
#                  db.user = gta_pwd("ricardodev")$user,
#                  db.password = gta_pwd("ricardodev")$password,
#                  table.prefix = "")

#gta_sql_pool_close()

bt_flush_conflicts=function(user.id = NULL,
                            is.superuser = NULL,
                            force.flush = NULL,
                            soft.flush = NULL){
  
  if(!is.numeric(user.id) & length(user.id)==1) return('user.id must be provided and of length 1')
  
  if(force.flush == T){
    
    ## this is a hard flush - accepts newest regardless of if altered in the past
    
    new.attributes = paste0("SELECT bt_conflict_date.hint_id, bt_conflict_date.conflict_date, bt_conflict_date.conflict_date_type_id FROM bt_conflict_date
                            JOIN (SELECT bt_conflict_date.hint_id, MAX(bt_conflict_date.conflict_id) AS newest_conflict, bt_conflict_date.conflict_status FROM bt_conflict_date WHERE bt_conflict_date.conflict_date_type_id = 1 GROUP BY hint_id) newest_conflict 
                            ON bt_conflict_date.conflict_date_type_id = 1 AND newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_date.conflict_id AND newest_conflict.conflict_status = 1;
                            
                            SELECT bt_conflict_date.hint_id, bt_conflict_date.conflict_date, bt_conflict_date.conflict_date_type_id FROM bt_conflict_date
                            JOIN (SELECT bt_conflict_date.hint_id, MAX(bt_conflict_date.conflict_id) AS newest_conflict, bt_conflict_date.conflict_status FROM bt_conflict_date WHERE bt_conflict_date.conflict_date_type_id = 2 GROUP BY hint_id) newest_conflict 
                            ON bt_conflict_date.conflict_date_type_id = 2 AND newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_date.conflict_id AND newest_conflict.conflict_status = 1;
                            
                            SELECT bt_conflict_date.hint_id, bt_conflict_date.conflict_date, bt_conflict_date.conflict_date_type_id FROM bt_conflict_date
                            JOIN (SELECT bt_conflict_date.hint_id, MAX(bt_conflict_date.conflict_id) AS newest_conflict, bt_conflict_date.conflict_status FROM bt_conflict_date WHERE bt_conflict_date.conflict_date_type_id = 3 GROUP BY hint_id) newest_conflict 
                            ON bt_conflict_date.conflict_date_type_id = 3 AND newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_date.conflict_id AND newest_conflict.conflict_status = 1;
                            
                            SELECT bt_conflict_assessment.hint_id, b221_assessment_list.assessment_name AS conflict_assessment_name FROM bt_conflict_assessment
                           	JOIN b221_assessment_list ON bt_conflict_assessment.conflict_assessment_id <=> b221_assessment_list.assessment_id 
                          	JOIN (SELECT bt_conflict_assessment.hint_id, MAX(bt_conflict_assessment.conflict_id) AS newest_conflict, bt_conflict_assessment.conflict_status FROM bt_conflict_assessment GROUP BY hint_id) newest_conflict 
                          	ON newest_conflict.hint_id = bt_conflict_assessment.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_assessment.conflict_id AND newest_conflict.conflict_status = 1;
                            
                            SELECT bt_conflict_intervention.hint_id, GROUP_CONCAT(DISTINCT (b221_intervention_type_list.intervention_type_name) SEPARATOR ' , ') AS conflict_intervention_name FROM bt_conflict_intervention
                          	JOIN b221_intervention_type_list ON b221_intervention_type_list.intervention_type_id <=> bt_conflict_intervention.conflict_intervention_id 
                          	JOIN (SELECT bt_conflict_intervention.hint_id, MAX(bt_conflict_intervention.conflict_id) AS newest_conflict, bt_conflict_intervention.conflict_status FROM bt_conflict_intervention GROUP BY hint_id) newest_conflict 
                          	ON newest_conflict.hint_id = bt_conflict_intervention.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_intervention.conflict_id AND newest_conflict.conflict_status = 1
                          	GROUP BY bt_conflict_intervention.hint_id;
                            
                            SELECT bt_conflict_jurisdiction.hint_id, GROUP_CONCAT(DISTINCT (bt_jurisdiction_list.jurisdiction_name) SEPARATOR ' , ') AS conflict_jurisdiction_name FROM bt_conflict_jurisdiction
                            JOIN bt_jurisdiction_list ON bt_jurisdiction_list.jurisdiction_id <=> bt_conflict_jurisdiction.conflict_jurisdiction_id 
                            JOIN (SELECT bt_conflict_jurisdiction.hint_id, MAX(bt_conflict_jurisdiction.conflict_id) AS newest_conflict, bt_conflict_jurisdiction.conflict_status FROM bt_conflict_jurisdiction GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_jurisdiction.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_jurisdiction.conflict_id AND newest_conflict.conflict_status = 1
                            GROUP BY bt_conflict_jurisdiction.hint_id;
                            
                            SELECT bt_conflict_product_group.hint_id, GROUP_CONCAT(DISTINCT (b221_product_group_list.product_group_name) SEPARATOR ' , ') AS conflict_product_group_name FROM bt_conflict_product_group
                            JOIN b221_product_group_list ON b221_product_group_list.product_group_id <=> bt_conflict_product_group.conflict_product_group_id 
                            JOIN (SELECT bt_conflict_product_group.hint_id, MAX(bt_conflict_product_group.conflict_id) AS newest_conflict, bt_conflict_product_group.conflict_status FROM bt_conflict_product_group GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_product_group.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_product_group.conflict_id AND newest_conflict.conflict_status = 1
                            GROUP BY bt_conflict_product_group.hint_id;
                            
                            SELECT bt_conflict_relevance.hint_id, bt_conflict_relevance.relevance FROM bt_conflict_relevance
                          	JOIN (SELECT bt_conflict_relevance.hint_id, MAX(bt_conflict_relevance.conflict_id) AS newest_conflict, bt_conflict_relevance.conflict_status FROM bt_conflict_relevance GROUP BY hint_id) newest_conflict 
                          	ON newest_conflict.hint_id = bt_conflict_relevance.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_relevance.conflict_id AND newest_conflict.conflict_status = 1;
                                                      
                            SELECT bt_conflict_text.hint_id, bt_conflict_text.conflict_description, bt_conflict_text.conflict_title FROM bt_conflict_text
                            JOIN (SELECT bt_conflict_text.hint_id, MAX(bt_conflict_text.conflict_id) AS newest_conflict, bt_conflict_text.conflict_status FROM bt_conflict_text GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_text.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_text.conflict_id AND newest_conflict.conflict_status = 1;
                            
                            SELECT bt_conflict_url.hint_id, bt_url_log.url AS conflict_url, bt_conflict_url.conflict_url_type_id FROM bt_conflict_url
                            JOIN bt_url_log ON bt_url_log.url_id <=> bt_conflict_url.conflict_url_id
                            JOIN (SELECT bt_conflict_url.hint_id, MAX(bt_conflict_url.conflict_id) AS newest_conflict, bt_conflict_url.conflict_status FROM bt_conflict_url GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_url.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_url.conflict_id AND newest_conflict.conflict_status = 1;")
    
    new.attributes = gta_sql_multiple_queries(new.attributes, output.queries = 1:10)
    names(new.attributes) = c('announcement','implementation','removal','assessment','intervention','jurisdiction','product','relevance','text','url')
    new.attributes <<- new.attributes
    
    pass.attributes <<- new.attributes$announcement %>%
           full_join(new.attributes$implementation, by = 'hint.id') %>%
           full_join(new.attributes$removal, by = 'hint.id') %>%
           full_join(new.attributes$assessment, by = 'hint.id') %>%
           full_join(new.attributes$intervention, by = 'hint.id') %>%
           full_join(new.attributes$jurisdiction, by = 'hint.id') %>%
           full_join(new.attributes$product, by = 'hint.id') %>%
           full_join(new.attributes$relevance, by = 'hint.id') %>%
           full_join(new.attributes$text, by = 'hint.id') %>%
           full_join(new.attributes$url, by = 'hint.id') %>%
           dplyr::rename(announcement.conflict.date = conflict.date.x, implementation.conflict.date = conflict.date.y, removal.conflict.date = conflict.date) %>%
           select(-c('conflict.date.type.id.x', 'conflict.date.type.id.y', 'conflict.date.type.id')) %>%
           mutate(conflict.intervention.name = str_split(conflict.intervention.name, ' , '),
                  conflict.jurisdiction.name = str_split(conflict.jurisdiction.name, ' , '),
                  conflict.product.group.name = str_split(conflict.product.group.name, ' , ')) %>%
           arrange(hint.id)
    
    pass.attributes.test <<- pass.attributes
    
    # check duplicates
    check <<- pass.attributes %>%
      group_by(hint.id) %>%
      filter(n() > 1)
    
    # b221_hint_change_attribute() here with the new attributes!
    pass.attributes %>%
      mutate(hints = hint.id) %>%
      group_by(hints) %>%
      nest() %>%
      map(as.list) %>%
      pluck('data') %>%
      map(.f = function(x){
        
        hint.intervention <- gta_sql_get_value(paste0("SELECT DISTINCT b221_intervention_type_list.intervention_type_id, b221_intervention_type_list.intervention_type_name FROM b221_intervention_type_list;"))
        hint.product <- gta_sql_get_value(paste0("SELECT DISTINCT b221_product_group_list.product_group_id, b221_product_group_list.product_group_name FROM b221_product_group_list;"))
        hint.jurisdiction <- gta_sql_get_value(paste0("SELECT DISTINCT gta_jurisdiction_list.jurisdiction_id, gta_jurisdiction_list.jurisdiction_name FROM gta_jurisdiction_list;"))
        
        
        b221_hint_change_attribute(change.id = x$hint.id,
                                  user.id = user.id,
                                  is.superuser = is.superuser,
                                  is.intervention = F,
                                  intervention.modifiable = F,
                                  modify.assessment = switch(!is.na(x$conflict.assessment.name), x$conflict.assessment.name, NULL),
                                  modify.is.official = switch(!is.na(x$conflict.url.type.id), x$conflict.url.type.id, NULL), 
                                  modify.date.announced = switch(!is.na(x$announcement.conflict.date), x$announcement.conflict.date, NULL), 
                                  modify.date.implemented = switch(!is.na(x$implementation.conflict.date), x$implementation.conflict.date, NULL), 
                                  modify.date.removed = switch(!is.na(x$removal.conflict.date), x$removal.conflict.date, NULL), 
                                  modify.relevance = switch(!is.na(x$relevance), x$relevance, NULL), 
                                  modify.title = switch(!is.na(x$conflict.title), x$conflict.title, NULL), 
                                  modify.description = switch(!is.na(x$conflict.description), x$conflict.description, NULL), 
                                  add.instrument = switch(!is.na(x$conflict.intervention.name), x$conflict.intervention.name, NULL), 
                                  remove.instrument = switch(!x$conflict.intervention.name %in% intervention.list)

                                  )
      })
      
    
    # update the status' 
    update.conflict.status = paste0("SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log' AND table_schema=DATABASE());
                              						
                                    INSERT INTO bt_classification_log(classification_id, user_id, hint_state_id, time_stamp)
                                    SELECT DISTINCT @classification_id AS classification_id, ",user.id," AS user_id, (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - freelancer desk') AS hint_state_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp; 
                                    
                                    UPDATE bt_conflict_date
                                    JOIN (SELECT bt_conflict_date.hint_id, MAX(bt_conflict_date.conflict_id) AS newest_conflict, bt_conflict_date.conflict_status FROM bt_conflict_date GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_date.conflict_status = 2, bt_conflict_date.resolution_classification = @classification_id;
                                    
                                    UPDATE bt_conflict_assessment
                                    JOIN (SELECT bt_conflict_assessment.hint_id, MAX(bt_conflict_assessment.conflict_id) AS newest_conflict, bt_conflict_assessment.conflict_status FROM bt_conflict_assessment GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_assessment.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_assessment.conflict_status = 2, bt_conflict_assessment.resolution_classification = @classification_id;
                                    
                                    UPDATE bt_conflict_intervention
                                    JOIN (SELECT bt_conflict_intervention.hint_id, MAX(bt_conflict_intervention.conflict_id) AS newest_conflict, bt_conflict_intervention.conflict_status FROM bt_conflict_intervention GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_intervention.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_intervention.conflict_status = 2, bt_conflict_intervention.resolution_classification = @classification_id;
                                    
                                    UPDATE bt_conflict_jurisdiction
                                    JOIN (SELECT bt_conflict_jurisdiction.hint_id, MAX(bt_conflict_jurisdiction.conflict_id) AS newest_conflict, bt_conflict_jurisdiction.conflict_status FROM bt_conflict_jurisdiction GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_jurisdiction.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_jurisdiction.conflict_status = 2, bt_conflict_jurisdiction.resolution_classification = @classification_id;
                                    
                                    UPDATE bt_conflict_product_group
                                    JOIN (SELECT bt_conflict_product_group.hint_id, MAX(bt_conflict_product_group.conflict_id) AS newest_conflict, bt_conflict_product_group.conflict_status FROM bt_conflict_product_group GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_product_group.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_product_group.conflict_status = 2, bt_conflict_product_group.resolution_classification = @classification_id;
                                    
                                    UPDATE bt_conflict_relevance
                                    JOIN (SELECT bt_conflict_relevance.hint_id, MAX(bt_conflict_relevance.conflict_id) AS newest_conflict, bt_conflict_relevance.conflict_status FROM bt_conflict_relevance GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_relevance.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_relevance.conflict_status = 2, bt_conflict_relevance.resolution_classification = @classification_id;
                                    
                                    UPDATE bt_conflict_text
                                    JOIN (SELECT bt_conflict_text.hint_id, MAX(bt_conflict_text.conflict_id) AS newest_conflict, bt_conflict_text.conflict_status FROM bt_conflict_text GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_text.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_text.conflict_status = 2, bt_conflict_text.resolution_classification = @classification_id;
                                    
                                    UPDATE bt_conflict_url
                                    JOIN (SELECT bt_conflict_url.hint_id, MAX(bt_conflict_url.conflict_id) AS newest_conflict, bt_conflict_url.conflict_status FROM bt_conflict_url GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_url.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_url.conflict_status = 2, bt_conflict_url.resolution_classification = @classification_id;")
    
    #update.conflicts = gta_sql_multiple_queries(update.conflict.status, output.queries = 1, show.time = T)
  }
  
  if(soft.flush == T){
    
    ## this is a soft flush - accepts newest only if unaltered in the past
    
    new.attributes = paste0("SELECT bt_conflict_date.conflict_id, bt_conflict_date.hint_id, bt_conflict_date.conflict_date, bt_conflict_date.conflict_date_type_id FROM bt_conflict_date
                            JOIN (SELECT bt_conflict_date.hint_id, MAX(bt_conflict_date.conflict_id) AS newest_conflict, bt_conflict_date.conflict_status FROM bt_conflict_date GROUP BY hint_id) newest_conflict 
                            ON bt_conflict_date.conflict_date_type_id = 1 AND newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_date.conflict_id AND newest_conflict.conflict_status = 1
                            WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_date resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                            
                            SELECT bt_conflict_date.conflict_id, bt_conflict_date.hint_id, bt_conflict_date.conflict_date, bt_conflict_date.conflict_date_type_id FROM bt_conflict_date
                            JOIN (SELECT bt_conflict_date.hint_id, MAX(bt_conflict_date.conflict_id) AS newest_conflict, bt_conflict_date.conflict_status FROM bt_conflict_date GROUP BY hint_id) newest_conflict 
                            ON bt_conflict_date.conflict_date_type_id = 2 AND newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_date.conflict_id AND newest_conflict.conflict_status = 1
                            WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_date resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                            
                            SELECT bt_conflict_date.conflict_id, bt_conflict_date.hint_id, bt_conflict_date.conflict_date, bt_conflict_date.conflict_date_type_id FROM bt_conflict_date
                            JOIN (SELECT bt_conflict_date.hint_id, MAX(bt_conflict_date.conflict_id) AS newest_conflict, bt_conflict_date.conflict_status FROM bt_conflict_date GROUP BY hint_id) newest_conflict 
                            ON bt_conflict_date.conflict_date_type_id = 3 AND newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_date.conflict_id AND newest_conflict.conflict_status = 1
                            WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_date resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                            
                            SELECT bt_conflict_assessment.conflict_id, bt_conflict_assessment.hint_id, bt_conflict_assessment.conflict_assessment_id FROM bt_conflict_assessment
                            JOIN (SELECT bt_conflict_assessment.hint_id, MAX(bt_conflict_assessment.conflict_id) AS newest_conflict, bt_conflict_assessment.conflict_status FROM bt_conflict_assessment GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_assessment.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_assessment.conflict_id AND newest_conflict.conflict_status = 1
                            WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_assessment resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                            
                            SELECT bt_conflict_intervention.conflict_id, bt_conflict_intervention.hint_id, bt_conflict_intervention.conflict_intervention_id FROM bt_conflict_intervention
                            JOIN (SELECT bt_conflict_intervention.hint_id, MAX(bt_conflict_intervention.conflict_id) AS newest_conflict, bt_conflict_intervention.conflict_status FROM bt_conflict_intervention GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_intervention.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_intervention.conflict_id AND newest_conflict.conflict_status = 1
                            WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_assessment resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                            
                            SELECT bt_conflict_jurisdiction.conflict_id, bt_conflict_jurisdiction.hint_id, bt_conflict_jurisdiction.conflict_jurisdiction_id FROM bt_conflict_jurisdiction
                            JOIN (SELECT bt_conflict_jurisdiction.hint_id, MAX(bt_conflict_jurisdiction.conflict_id) AS newest_conflict, bt_conflict_jurisdiction.conflict_status FROM bt_conflict_jurisdiction GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_jurisdiction.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_jurisdiction.conflict_id AND newest_conflict.conflict_status = 1
                            WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_jurisdiction resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                            
                            SELECT bt_conflict_product_group.conflict_id, bt_conflict_product_group.hint_id, bt_conflict_product_group.conflict_product_group_id FROM bt_conflict_product_group
                            JOIN (SELECT bt_conflict_product_group.hint_id, MAX(bt_conflict_product_group.conflict_id) AS newest_conflict, bt_conflict_product_group.conflict_status FROM bt_conflict_product_group GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_product_group.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_product_group.conflict_id AND newest_conflict.conflict_status = 1
                            WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_product_group resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                            
                            SELECT bt_conflict_relevance.conflict_id, bt_conflict_relevance.hint_id, bt_conflict_relevance.relevance FROM bt_conflict_relevance
                            JOIN (SELECT bt_conflict_relevance.hint_id, MAX(bt_conflict_relevance.conflict_id) AS newest_conflict, bt_conflict_relevance.conflict_status FROM bt_conflict_relevance GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_relevance.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_relevance.conflict_id AND newest_conflict.conflict_status = 1
                            WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_relevance resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                            
                            SELECT bt_conflict_text.conflict_id, bt_conflict_text.hint_id, bt_conflict_text.conflict_description, bt_conflict_text.conflict_title FROM bt_conflict_text
                            JOIN (SELECT bt_conflict_text.hint_id, MAX(bt_conflict_text.conflict_id) AS newest_conflict, bt_conflict_text.conflict_status FROM bt_conflict_text GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_text.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_text.conflict_id AND newest_conflict.conflict_status = 1
                            WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_text resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                            
                            SELECT bt_conflict_url.conflict_id, bt_conflict_url.hint_id, bt_conflict_url.conflict_url_id, bt_conflict_url.conflict_url_type_id FROM bt_conflict_url
                            JOIN (SELECT bt_conflict_url.hint_id, MAX(bt_conflict_url.conflict_id) AS newest_conflict, bt_conflict_url.conflict_status FROM bt_conflict_url GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_url.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_url.conflict_id AND newest_conflict.conflict_status = 1
                            WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_url resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);")
    
    new.attributes = gta_sql_multiple_queries(new.attributes, output.queries = 1:10)
    names(new.attributes) = c('announcement','implementation','removal','assessment','intervention','jurisdiction','product','relevance','text','url')

    # b221_hint_change_attribute() here with the new attributes!
    
    # update the status' 
    update.conflict.status = paste0("SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log' AND table_schema=DATABASE());
                              						
                                    INSERT INTO bt_classification_log(classification_id, user_id, hint_state_id, time_stamp)
                                    SELECT DISTINCT @classification_id AS classification_id, ",user.id," AS user_id, (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - freelancer desk') AS hint_state_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp; 
                                    
                                    UPDATE bt_conflict_date
                                    JOIN (SELECT bt_conflict_date.hint_id, MAX(bt_conflict_date.conflict_id) AS newest_conflict, bt_conflict_date.conflict_status FROM bt_conflict_date GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_date.conflict_status = 2, bt_conflict_date.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM (SELECT * FROM bt_conflict_date) AS resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 2);
                                    
                                    UPDATE bt_conflict_assessment
                                    JOIN (SELECT bt_conflict_assessment.hint_id, MAX(bt_conflict_assessment.conflict_id) AS newest_conflict, bt_conflict_assessment.conflict_status FROM bt_conflict_assessment GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_assessment.conflict_status = 2, bt_conflict_assessment.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM (SELECT * FROM bt_conflict_assessment) AS resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 2);
                                    
                                    UPDATE bt_conflict_intervention
                                    JOIN (SELECT bt_conflict_intervention.hint_id, MAX(bt_conflict_intervention.conflict_id) AS newest_conflict, bt_conflict_intervention.conflict_status FROM bt_conflict_intervention GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_intervention.conflict_status = 2, bt_conflict_intervention.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM (SELECT * FROM bt_conflict_intervention) AS resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 2);
                                    
                                    UPDATE bt_conflict_jurisdiction
                                    JOIN (SELECT bt_conflict_jurisdiction.hint_id, MAX(bt_conflict_jurisdiction.conflict_id) AS newest_conflict, bt_conflict_jurisdiction.conflict_status FROM bt_conflict_jurisdiction GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_jurisdiction.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_jurisdiction.conflict_status = 2, bt_conflict_jurisdiction.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM (SELECT * FROM bt_conflict_jurisdiction) AS resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 2);
                                    
                                    UPDATE bt_conflict_product_group
                                    JOIN (SELECT bt_conflict_product_group.hint_id, MAX(bt_conflict_product_group.conflict_id) AS newest_conflict, bt_conflict_product_group.conflict_status FROM bt_conflict_product_group GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_product_group.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_product_group.conflict_status = 2, bt_conflict_product_group.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM (SELECT * FROM bt_conflict_product_group) AS resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 2);
                                    
                                    UPDATE bt_conflict_relevance
                                    JOIN (SELECT bt_conflict_relevance.hint_id, MAX(bt_conflict_relevance.conflict_id) AS newest_conflict, bt_conflict_relevance.conflict_status FROM bt_conflict_relevance GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_relevance.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_relevance.conflict_status = 2, bt_conflict_relevance.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM (SELECT * FROM bt_conflict_relevance) AS resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 2);
                                    
                                    UPDATE bt_conflict_text
                                    JOIN (SELECT bt_conflict_text.hint_id, MAX(bt_conflict_text.conflict_id) AS newest_conflict, bt_conflict_text.conflict_status FROM bt_conflict_text GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_text.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_text.conflict_status = 2, bt_conflict_text.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM (SELECT * FROM bt_conflict_text) AS resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 2);
                                    
                                    UPDATE bt_conflict_url
                                    JOIN (SELECT bt_conflict_url.hint_id, MAX(bt_conflict_url.conflict_id) AS newest_conflict, bt_conflict_url.conflict_status FROM bt_conflict_url GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_url.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_url.conflict_status = 2, bt_conflict_url.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM (SELECT * FROM bt_conflict_url) AS resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 2);")
    
    #update.conflicts = gta_sql_multiple_queries(update.conflict.status, output.queries = 1, show.time = T)
  }
  
  
}
