# library(pool)
# library(gtasql)
# library(gtalibrary)
# library(tidyverse)
# library(plyr)
# gta_setwd()

gta_sql_pool_open(db.title="ricardodev",
                  db.host = gta_pwd("ricardodev")$host,
                  db.name = "dlvr_app_extension_dev",
                  db.user = gta_pwd("ricardodev")$user,
                  db.password = gta_pwd("ricardodev")$password,
                  table.prefix = "")

gta_sql_pool_close()

bt_flush_conflicts=function(user.id = NULL,
                            force.flush = NULL,
                            soft.flush = NULL){
  
  if(!is.numeric(user.id) & length(user.id)==1) return('user.id must be provided and of length 1')
  
  if(force.flush == T){
    
    ## this is a hard flush - accepts newest regardless of if altered in the past
    
    new.attributes = paste0("SELECT bt_conflict_date.conflict_id, bt_conflict_date.hint_id, bt_conflict_date.conflict_date, bt_conflict_date.conflict_date_type_id FROM bt_conflict_date
                            JOIN (SELECT bt_conflict_date.hint_id, MAX(bt_conflict_date.conflict_id) AS newest_conflict, bt_conflict_date.conflict_status FROM bt_conflict_date GROUP BY hint_id) newest_conflict 
                            ON bt_conflict_date.conflict_date_type_id = 1 AND newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_date.conflict_id AND newest_conflict.conflict_status = 1;
                            
                            SELECT bt_conflict_date.conflict_id, bt_conflict_date.hint_id, bt_conflict_date.conflict_date, bt_conflict_date.conflict_date_type_id FROM bt_conflict_date
                            JOIN (SELECT bt_conflict_date.hint_id, MAX(bt_conflict_date.conflict_id) AS newest_conflict, bt_conflict_date.conflict_status FROM bt_conflict_date GROUP BY hint_id) newest_conflict 
                            ON bt_conflict_date.conflict_date_type_id = 2 AND newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_date.conflict_id AND newest_conflict.conflict_status = 1;
                            
                            SELECT bt_conflict_date.conflict_id, bt_conflict_date.hint_id, bt_conflict_date.conflict_date, bt_conflict_date.conflict_date_type_id FROM bt_conflict_date
                            JOIN (SELECT bt_conflict_date.hint_id, MAX(bt_conflict_date.conflict_id) AS newest_conflict, bt_conflict_date.conflict_status FROM bt_conflict_date GROUP BY hint_id) newest_conflict 
                            ON bt_conflict_date.conflict_date_type_id = 3 AND newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_date.conflict_id AND newest_conflict.conflict_status = 1;
                            
                            SELECT bt_conflict_assessment.conflict_id, bt_conflict_assessment.hint_id, bt_conflict_assessment.conflict_assessment_id FROM bt_conflict_assessment
                            JOIN (SELECT bt_conflict_assessment.hint_id, MAX(bt_conflict_assessment.conflict_id) AS newest_conflict, bt_conflict_assessment.conflict_status FROM bt_conflict_assessment GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_assessment.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_assessment.conflict_id AND newest_conflict.conflict_status = 1;
                            
                            SELECT bt_conflict_intervention.conflict_id, bt_conflict_intervention.hint_id, bt_conflict_intervention.conflict_intervention_id FROM bt_conflict_intervention
                            JOIN (SELECT bt_conflict_intervention.hint_id, MAX(bt_conflict_intervention.conflict_id) AS newest_conflict, bt_conflict_intervention.conflict_status FROM bt_conflict_intervention GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_intervention.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_intervention.conflict_id AND newest_conflict.conflict_status = 1;
                            
                            SELECT bt_conflict_jurisdiction.conflict_id, bt_conflict_jurisdiction.hint_id, bt_conflict_jurisdiction.conflict_jurisdiction_id FROM bt_conflict_jurisdiction
                            JOIN (SELECT bt_conflict_jurisdiction.hint_id, MAX(bt_conflict_jurisdiction.conflict_id) AS newest_conflict, bt_conflict_jurisdiction.conflict_status FROM bt_conflict_jurisdiction GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_jurisdiction.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_jurisdiction.conflict_id AND newest_conflict.conflict_status = 1;
                            
                            SELECT bt_conflict_product_group.conflict_id, bt_conflict_product_group.hint_id, bt_conflict_product_group.conflict_product_group_id FROM bt_conflict_product_group
                            JOIN (SELECT bt_conflict_product_group.hint_id, MAX(bt_conflict_product_group.conflict_id) AS newest_conflict, bt_conflict_product_group.conflict_status FROM bt_conflict_product_group GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_product_group.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_product_group.conflict_id AND newest_conflict.conflict_status = 1;
                            
                            SELECT bt_conflict_relevance.conflict_id, bt_conflict_relevance.hint_id, bt_conflict_relevance.relevance FROM bt_conflict_relevance
                            JOIN (SELECT bt_conflict_relevance.hint_id, MAX(bt_conflict_relevance.conflict_id) AS newest_conflict, bt_conflict_relevance.conflict_status FROM bt_conflict_relevance GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_relevance.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_relevance.conflict_id AND newest_conflict.conflict_status = 1;
                            
                            SELECT bt_conflict_text.conflict_id, bt_conflict_text.hint_id, bt_conflict_text.conflict_description, bt_conflict_text.conflict_title FROM bt_conflict_text
                            JOIN (SELECT bt_conflict_text.hint_id, MAX(bt_conflict_text.conflict_id) AS newest_conflict, bt_conflict_text.conflict_status FROM bt_conflict_text GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_text.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_text.conflict_id AND newest_conflict.conflict_status = 1;
                            
                            SELECT bt_conflict_url.conflict_id, bt_conflict_url.hint_id, bt_conflict_url.conflict_url_id, bt_conflict_url.conflict_url_type_id FROM bt_conflict_url
                            JOIN (SELECT bt_conflict_url.hint_id, MAX(bt_conflict_url.conflict_id) AS newest_conflict, bt_conflict_url.conflict_status FROM bt_conflict_url GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_url.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_url.conflict_id AND newest_conflict.conflict_status = 1;")
    
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
                                    SET bt_conflict_date.conflict_status = (CASE WHEN bt_conflict_date.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_date.resolution_classification = @classification_id;
                                    
                                    UPDATE bt_conflict_assessment
                                    JOIN (SELECT bt_conflict_assessment.hint_id, MAX(bt_conflict_assessment.conflict_id) AS newest_conflict, bt_conflict_assessment.conflict_status FROM bt_conflict_assessment GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_assessment.conflict_status = (CASE WHEN bt_conflict_assessment.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_assessment.resolution_classification = @classification_id;
                                    
                                    UPDATE bt_conflict_intervention
                                    JOIN (SELECT bt_conflict_intervention.hint_id, MAX(bt_conflict_intervention.conflict_id) AS newest_conflict, bt_conflict_intervention.conflict_status FROM bt_conflict_intervention GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_intervention.conflict_status = (CASE WHEN bt_conflict_intervention.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_intervention.resolution_classification = @classification_id;
                                    
                                    UPDATE bt_conflict_jurisdiction
                                    JOIN (SELECT bt_conflict_jurisdiction.hint_id, MAX(bt_conflict_jurisdiction.conflict_id) AS newest_conflict, bt_conflict_jurisdiction.conflict_status FROM bt_conflict_jurisdiction GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_jurisdiction.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_jurisdiction.conflict_status = (CASE WHEN bt_conflict_jurisdiction.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_jurisdiction.resolution_classification = @classification_id;
                                    
                                    UPDATE bt_conflict_product_group
                                    JOIN (SELECT bt_conflict_product_group.hint_id, MAX(bt_conflict_product_group.conflict_id) AS newest_conflict, bt_conflict_product_group.conflict_status FROM bt_conflict_product_group GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_product_group.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_product_group.conflict_status = (CASE WHEN bt_conflict_product_group.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_product_group.resolution_classification = @classification_id;
                                    
                                    UPDATE bt_conflict_relevance
                                    JOIN (SELECT bt_conflict_relevance.hint_id, MAX(bt_conflict_relevance.conflict_id) AS newest_conflict, bt_conflict_relevance.conflict_status FROM bt_conflict_relevance GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_relevance.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_relevance.conflict_status = (CASE WHEN bt_conflict_relevance.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_relevance.resolution_classification = @classification_id;
                                    
                                    UPDATE bt_conflict_text
                                    JOIN (SELECT bt_conflict_text.hint_id, MAX(bt_conflict_text.conflict_id) AS newest_conflict, bt_conflict_text.conflict_status FROM bt_conflict_text GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_text.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_text.conflict_status = (CASE WHEN bt_conflict_text.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_text.resolution_classification = @classification_id;
                                    
                                    UPDATE bt_conflict_url
                                    JOIN (SELECT bt_conflict_url.hint_id, MAX(bt_conflict_url.conflict_id) AS newest_conflict, bt_conflict_url.conflict_status FROM bt_conflict_url GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_url.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_url.conflict_status = (CASE WHEN bt_conflict_url.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_url.resolution_classification = @classification_id;")
    
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
                                    SET bt_conflict_date.conflict_status = (CASE WHEN bt_conflict_date.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_date.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_date resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                                    
                                    UPDATE bt_conflict_assessment
                                    JOIN (SELECT bt_conflict_assessment.hint_id, MAX(bt_conflict_assessment.conflict_id) AS newest_conflict, bt_conflict_assessment.conflict_status FROM bt_conflict_assessment GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_assessment.conflict_status = (CASE WHEN bt_conflict_assessment.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_assessment.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_assessment resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                                    
                                    UPDATE bt_conflict_intervention
                                    JOIN (SELECT bt_conflict_intervention.hint_id, MAX(bt_conflict_intervention.conflict_id) AS newest_conflict, bt_conflict_intervention.conflict_status FROM bt_conflict_intervention GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_intervention.conflict_status = (CASE WHEN bt_conflict_intervention.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_intervention.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_assessment resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                                    
                                    UPDATE bt_conflict_jurisdiction
                                    JOIN (SELECT bt_conflict_jurisdiction.hint_id, MAX(bt_conflict_jurisdiction.conflict_id) AS newest_conflict, bt_conflict_jurisdiction.conflict_status FROM bt_conflict_jurisdiction GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_jurisdiction.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_jurisdiction.conflict_status = (CASE WHEN bt_conflict_jurisdiction.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_jurisdiction.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_jurisdiction resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                                    
                                    UPDATE bt_conflict_product_group
                                    JOIN (SELECT bt_conflict_product_group.hint_id, MAX(bt_conflict_product_group.conflict_id) AS newest_conflict, bt_conflict_product_group.conflict_status FROM bt_conflict_product_group GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_product_group.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_product_group.conflict_status = (CASE WHEN bt_conflict_product_group.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_product_group.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_product_group resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                                    
                                    UPDATE bt_conflict_relevance
                                    JOIN (SELECT bt_conflict_relevance.hint_id, MAX(bt_conflict_relevance.conflict_id) AS newest_conflict, bt_conflict_relevance.conflict_status FROM bt_conflict_relevance GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_relevance.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_relevance.conflict_status = (CASE WHEN bt_conflict_relevance.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_relevance.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_relevance resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                                    
                                    UPDATE bt_conflict_text
                                    JOIN (SELECT bt_conflict_text.hint_id, MAX(bt_conflict_text.conflict_id) AS newest_conflict, bt_conflict_text.conflict_status FROM bt_conflict_text GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_text.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_text.conflict_status = (CASE WHEN bt_conflict_text.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_text.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_text resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                                    
                                    UPDATE bt_conflict_url
                                    JOIN (SELECT bt_conflict_url.hint_id, MAX(bt_conflict_url.conflict_id) AS newest_conflict, bt_conflict_url.conflict_status FROM bt_conflict_url GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_url.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_url.conflict_status = (CASE WHEN bt_conflict_url.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END), bt_conflict_url.resolution_classification = @classification_id
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_url resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);")
    
  }
  
  
}