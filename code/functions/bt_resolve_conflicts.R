library(pool)
library(gtasql)
library(gtalibrary)
library(tidyverse)
library(plyr)
gta_setwd()

gta_sql_pool_open(db.title="ricardodev",
                  db.host = gta_pwd("ricardodev")$host,
                  db.name = "dlvr_app_extension_dev",
                  db.user = gta_pwd("ricardodev")$user,
                  db.password = gta_pwd("ricardodev")$password,
                  table.prefix = "")



bt_resolve_conflicts=function(change.id=NULL,
                              is.intervention=F,
                              add.instrument=NULL,
                              remove.instrument=NULL,
                              add.assessment=NULL,
                              remove.assessment=NULL,
                              add.product=NULL,
                              remove.product=NULL,
                              add.jurisdiction=NULL,
                              remove.jurisdiction=NULL,
                              add.date.announced=NULL,
                              remove.date.announced=NULL,
                              add.date.implemented=NULL,
                              remove.date.implemented=NULL,
                              add.date.removed=NULL,
                              remove.date.removed=NULL,
                              hint.relevance=NULL,
                              hint.title=NULL,
                              hint.description=NULL,
                              force.accept.newest.attributes = NULL,
                              accept.newest.attributes = NULL){
  
  if(force.accept.newest.attributes == T){
    
    ## this is a hard flush - accepts newest regardless of if altered in the past
    
    new.attributes = paste0("SELECT bt_conflict_date.conflict_id, bt_conflict_date.hint_id, bt_conflict_date.conflict_date, bt_conflict_date.conflict_date_type_id FROM bt_conflict_date
                            JOIN (SELECT bt_conflict_date.hint_id, MAX(bt_conflict_date.conflict_id) AS newest_conflict, bt_conflict_date.conflict_status FROM bt_conflict_date GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_date.conflict_id AND newest_conflict.conflict_status = 1;
                            
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
    
    new.attributes = gta_sql_multiple_queries(new.attributes, output.queries = 1:8)
    names(new.attributes) = c('date','assessment','intervention','jurisdiction','product','relevance','text','url')
    
    
    # add backlog of conflict based on prior value for the "record"
    add.conflict = paste0("SET @conflict_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_conflict_log' AND TABLE_SCHEMA = DATABASE());
                                                        
                          INSERT INTO bt_conflict_log(conflict_id, conflict_creation)
                          SELECT @conflict_id AS conflict_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS conflict_creation; 
                          
                          ")
    
    # b221_hint_change_attribute() here with the new attributes!
    
    # update the status' 
    update.conflict.status = paste0("UPDATE bt_conflict_date
                                    JOIN (SELECT bt_conflict_date.hint_id, MAX(bt_conflict_date.conflict_id) AS newest_conflict, bt_conflict_date.conflict_status FROM bt_conflict_date GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_date.conflict_status = (CASE WHEN bt_conflict_date.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END);
                                    
                                    UPDATE bt_conflict_assessment
                                    JOIN (SELECT bt_conflict_assessment.hint_id, MAX(bt_conflict_assessment.conflict_id) AS newest_conflict, bt_conflict_assessment.conflict_status FROM bt_conflict_assessment GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_assessment.conflict_status = (CASE WHEN bt_conflict_assessment.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END);
                                    
                                    UPDATE bt_conflict_intervention
                                    JOIN (SELECT bt_conflict_intervention.hint_id, MAX(bt_conflict_intervention.conflict_id) AS newest_conflict, bt_conflict_intervention.conflict_status FROM bt_conflict_intervention GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_intervention.conflict_status = (CASE WHEN bt_conflict_intervention.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END);
                                    
                                    UPDATE bt_conflict_jurisdiction
                                    JOIN (SELECT bt_conflict_jurisdiction.hint_id, MAX(bt_conflict_jurisdiction.conflict_id) AS newest_conflict, bt_conflict_jurisdiction.conflict_status FROM bt_conflict_jurisdiction GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_jurisdiction.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_jurisdiction.conflict_status = (CASE WHEN bt_conflict_jurisdiction.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END);
                                    
                                    UPDATE bt_conflict_product_group
                                    JOIN (SELECT bt_conflict_product_group.hint_id, MAX(bt_conflict_product_group.conflict_id) AS newest_conflict, bt_conflict_product_group.conflict_status FROM bt_conflict_product_group GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_product_group.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_product_group.conflict_status = (CASE WHEN bt_conflict_product_group.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END);
                                    
                                    UPDATE bt_conflict_relevance
                                    JOIN (SELECT bt_conflict_relevance.hint_id, MAX(bt_conflict_relevance.conflict_id) AS newest_conflict, bt_conflict_relevance.conflict_status FROM bt_conflict_relevance GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_relevance.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_relevance.conflict_status = (CASE WHEN bt_conflict_relevance.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END);
                                    
                                    UPDATE bt_conflict_text
                                    JOIN (SELECT bt_conflict_text.hint_id, MAX(bt_conflict_text.conflict_id) AS newest_conflict, bt_conflict_text.conflict_status FROM bt_conflict_text GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_text.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_text.conflict_status = (CASE WHEN bt_conflict_text.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END);
                                    
                                    UPDATE bt_conflict_url
                                    JOIN (SELECT bt_conflict_url.hint_id, MAX(bt_conflict_url.conflict_id) AS newest_conflict, bt_conflict_url.conflict_status FROM bt_conflict_url GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_url.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_url.conflict_status = (CASE WHEN bt_conflict_url.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END);")
    
  }
  
  if(accept.newest.attributes == T){
    
    ## this is a hard flush - accepts newest regardless of if altered in the past
    
    new.attributes = paste0("SELECT bt_conflict_date.conflict_id, bt_conflict_date.hint_id, bt_conflict_date.conflict_date, bt_conflict_date.conflict_date_type_id FROM bt_conflict_date
                            JOIN (SELECT bt_conflict_date.hint_id, MAX(bt_conflict_date.conflict_id) AS newest_conflict, bt_conflict_date.conflict_status FROM bt_conflict_date GROUP BY hint_id) newest_conflict 
                            ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.newest_conflict <=> bt_conflict_date.conflict_id AND newest_conflict.conflict_status = 1
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
    
    new.attributes = gta_sql_multiple_queries(new.attributes, output.queries = 1:8)
    names(new.attributes) = c('date','assessment','intervention','jurisdiction','product','relevance','text','url')
    
    
    # add backlog of conflict based on prior value for the "record"
    add.conflict = paste0("SET @conflict_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_conflict_log' AND TABLE_SCHEMA = DATABASE());
                          
                          INSERT INTO bt_conflict_log(conflict_id, conflict_creation)
                          SELECT @conflict_id AS conflict_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS conflict_creation; 
                          
                          ")
    
    # b221_hint_change_attribute() here with the new attributes!
    
    # update the status' 
    update.conflict.status = paste0("UPDATE bt_conflict_date
                                    JOIN (SELECT bt_conflict_date.hint_id, MAX(bt_conflict_date.conflict_id) AS newest_conflict, bt_conflict_date.conflict_status FROM bt_conflict_date GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_date.conflict_status = (CASE WHEN bt_conflict_date.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END)
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_date resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                                    
                                    UPDATE bt_conflict_assessment
                                    JOIN (SELECT bt_conflict_assessment.hint_id, MAX(bt_conflict_assessment.conflict_id) AS newest_conflict, bt_conflict_assessment.conflict_status FROM bt_conflict_assessment GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_assessment.conflict_status = (CASE WHEN bt_conflict_assessment.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END)
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_assessment resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                                    
                                    UPDATE bt_conflict_intervention
                                    JOIN (SELECT bt_conflict_intervention.hint_id, MAX(bt_conflict_intervention.conflict_id) AS newest_conflict, bt_conflict_intervention.conflict_status FROM bt_conflict_intervention GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_date.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_intervention.conflict_status = (CASE WHEN bt_conflict_intervention.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END)
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_assessment resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                                    
                                    UPDATE bt_conflict_jurisdiction
                                    JOIN (SELECT bt_conflict_jurisdiction.hint_id, MAX(bt_conflict_jurisdiction.conflict_id) AS newest_conflict, bt_conflict_jurisdiction.conflict_status FROM bt_conflict_jurisdiction GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_jurisdiction.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_jurisdiction.conflict_status = (CASE WHEN bt_conflict_jurisdiction.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END)
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_jurisdiction resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                                    
                                    UPDATE bt_conflict_product_group
                                    JOIN (SELECT bt_conflict_product_group.hint_id, MAX(bt_conflict_product_group.conflict_id) AS newest_conflict, bt_conflict_product_group.conflict_status FROM bt_conflict_product_group GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_product_group.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_product_group.conflict_status = (CASE WHEN bt_conflict_product_group.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END)
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_product_group resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                                    
                                    UPDATE bt_conflict_relevance
                                    JOIN (SELECT bt_conflict_relevance.hint_id, MAX(bt_conflict_relevance.conflict_id) AS newest_conflict, bt_conflict_relevance.conflict_status FROM bt_conflict_relevance GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_relevance.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_relevance.conflict_status = (CASE WHEN bt_conflict_relevance.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END)
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_relevance resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                                    
                                    UPDATE bt_conflict_text
                                    JOIN (SELECT bt_conflict_text.hint_id, MAX(bt_conflict_text.conflict_id) AS newest_conflict, bt_conflict_text.conflict_status FROM bt_conflict_text GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_text.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_text.conflict_status = (CASE WHEN bt_conflict_text.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END)
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_text resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);
                                    
                                    UPDATE bt_conflict_url
                                    JOIN (SELECT bt_conflict_url.hint_id, MAX(bt_conflict_url.conflict_id) AS newest_conflict, bt_conflict_url.conflict_status FROM bt_conflict_url GROUP BY hint_id) newest_conflict 
                                    ON newest_conflict.hint_id = bt_conflict_url.hint_id AND newest_conflict.conflict_status = 1
                                    SET bt_conflict_url.conflict_status = (CASE WHEN bt_conflict_url.conflict_id <=> newest_conflict.newest_conflict THEN 3 ELSE 2 END)
                                    WHERE NOT EXISTS (SELECT NULL FROM bt_conflict_url resolved_conflicts WHERE resolved_conflicts.hint_id = newest_conflict.hint_id AND resolved_conflicts.conflict_status = 3);")
    
  }
  
  
}