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

gta_sql_pool_open(db.title="ricardomainclone",
                  db.host = "gta-ricardo-dev.cp7esvs8xwum.eu-west-1.rds.amazonaws.com",
                  db.name = 'dlvr_app_extension_dev',
                  db.user = 'gtaricardodev',
                  db.password = '4rbjDVRote7YLsTqfmWXfbwdf7jVt8VjwXUhgy',
                  table.prefix = "")

gta_sql_pool_close()

change.id = 1


## HINT level
b221_hint_change_attribute<-function(change.id=NULL,
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
                                     hint.description=NULL){
  
  # find hint id if intervention id is what is provided
  if(is.intervention){
    change.id=unique(gta_sql_get_value("SELECT DISTINCT hint_id FROM bt_hint_log WHERE gta_id = ",change.id,";"))
    change.id=change.id[!is.na(change.id)]
    if(length(change.id)<1) return("no such intervention id was found")
  }
  
  col.id=gta_sql_get_value(paste0("SELECT DISTINCT hint_id, collection_id FROM b221_hint_collection WHERE hint_id IN (",paste0(change.id, collapse=','),");")) #paste0("SELECT DISTINCT hint_id, collection_id FROM b221_hint_collection WHERE hint_id = ",change.id,";")
  test_col.id <<- col.id
  # find attributes pre-change
  pull.hint.attributes = sprintf(paste0("SELECT change_ids.hint_id, ht_ass.assessment_id, ht_int.apparent_intervention_id AS intervention_id, 
                                        prod_grp.product_group_id, ht_jur.jurisdiction_id, ht_rlvnt.relevance, bt_hint_date.`date`, bt_hint_date.date_type_id FROM 
                                        (SELECT bt_hint_log.hint_id FROM bt_hint_log WHERE bt_hint_log.hint_id IN (%s)) change_ids 
                                        LEFT JOIN (SELECT b221_hint_assessment.hint_id, b221_hint_assessment.assessment_id, b221_hint_assessment.assessment_accepted FROM b221_hint_assessment JOIN (SELECT b221_hint_assessment.hint_id, MAX(b221_hint_assessment.validation_classification) AS newest_classification FROM b221_hint_assessment GROUP BY hint_id) newest_classification ON newest_classification.hint_id = b221_hint_assessment.hint_id AND newest_classification.newest_classification <=> b221_hint_assessment.validation_classification) ht_ass ON ht_ass.hint_id = change_ids.hint_id AND ht_ass.assessment_accepted = 1
                                        LEFT JOIN (SELECT b221_hint_intervention.hint_id, b221_hint_intervention.apparent_intervention_id, b221_hint_intervention.intervention_accepted FROM b221_hint_intervention JOIN (SELECT b221_hint_intervention.hint_id, MAX(b221_hint_intervention.validation_classification) AS newest_classification FROM b221_hint_intervention GROUP BY hint_id) newest_classification ON newest_classification.hint_id = b221_hint_intervention.hint_id AND newest_classification.newest_classification <=> b221_hint_intervention.validation_classification) ht_int ON ht_int.hint_id = change_ids.hint_id AND ht_int.intervention_accepted = 1
                                        LEFT JOIN (SELECT b221_hint_product_group.hint_id, b221_hint_product_group.product_group_id, b221_hint_product_group.product_group_assessment FROM b221_hint_product_group JOIN (SELECT b221_hint_product_group.hint_id, MAX(b221_hint_product_group.validation_classification) AS newest_classification FROM b221_hint_product_group GROUP BY hint_id) newest_classification ON newest_classification.hint_id = b221_hint_product_group.hint_id AND newest_classification.newest_classification <=> b221_hint_product_group.validation_classification) prod_grp ON prod_grp.hint_id = change_ids.hint_id AND prod_grp.product_group_assessment = 1
                                        LEFT JOIN (SELECT bt_hint_jurisdiction.hint_id, bt_hint_jurisdiction.jurisdiction_id, bt_hint_jurisdiction.jurisdiction_accepted FROM bt_hint_jurisdiction JOIN (SELECT bt_hint_jurisdiction.hint_id, MAX(bt_hint_jurisdiction.validation_classification) AS newest_classification FROM bt_hint_jurisdiction GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_jurisdiction.hint_id AND newest_classification.newest_classification <=> bt_hint_jurisdiction.validation_classification) ht_jur ON ht_jur.hint_id = change_ids.hint_id AND ht_jur.jurisdiction_accepted = 1
                                        LEFT JOIN (SELECT bt_hint_relevance.hint_id, bt_hint_relevance.relevance, bt_hint_relevance.relevance_accepted FROM bt_hint_relevance JOIN (SELECT bt_hint_relevance.hint_id, MAX(bt_hint_relevance.validation_classification) AS newest_classification FROM bt_hint_relevance GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_relevance.hint_id AND newest_classification.newest_classification <=> bt_hint_relevance.validation_classification) ht_rlvnt ON ht_rlvnt.hint_id = change_ids.hint_id AND ht_rlvnt.relevance_accepted = 1
                                        LEFT JOIN (SELECT bt_hint_date.hint_id, bt_hint_date.`date`, bt_hint_date.date_type_id, bt_hint_date.date_accepted FROM bt_hint_date JOIN (SELECT bt_hint_date.hint_id, MAX(bt_hint_date.validation_classification) AS newest_classification FROM bt_hint_date GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_date.hint_id AND newest_classification.newest_classification <=> bt_hint_date.validation_classification) bt_hint_date ON bt_hint_date.hint_id = change_ids.hint_id AND bt_hint_date.date_accepted = 1;"),ifelse(paste0(change.id, collapse = ',')=='',"NULL",paste0(change.id, collapse = ',')))
  pull.hint.attributes = gta_sql_get_value(pull.hint.attributes)
  #pull.hint.attributes = merge(pull.hint.attributes, col.id, by = 'hint.id')
  
  #add dfs with id and names of the attributes
  assessment.list <- gta_sql_get_value(paste0("SELECT DISTINCT bal.assessment_id , bal.assessment_name FROM b221_assessment_list bal WHERE bal.assessment_id IN(",paste0(unique(pull.hint.attributes$assessment.id), collapse=','),");"))
  intervention.list <- gta_sql_get_value(paste0("SELECT DISTINCT bitl.intervention_type_id, bitl.intervention_type_name FROM b221_intervention_type_list bitl WHERE bitl.intervention_type_id IN (", paste0(unique(pull.hint.attributes$intervention.id),collapse=',') ,");"))
  product.list <- gta_sql_get_value(paste0("SELECT DISTINCT bpgl.product_group_id, bpgl.product_group_name FROM b221_product_group_list bpgl WHERE bpgl.product_group_id IN (", paste0(unique(pull.hint.attributes$product.group.id),collapse=',') ,");"))
  jurisdiction.list <- gta_sql_get_value(paste0("SELECT DISTINCT gjl.jurisdiction_id, gjl.jurisdiction_name FROM gta_jurisdiction_list gjl WHERE gjl.jurisdiction_id IN (", paste0(unique(pull.hint.attributes$jurisdiction.id),collapse=',') ,");"))

  test_pull.attributes <<- pull.hint.attributes

  pull.hint.attributes <- pull.hint.attributes %>% 
                              left_join(col.id, by = 'hint.id')
 
  pass.hint.attributes <- 
    pull.hint.attributes %>%
    mutate(announcementdate = ifelse(date.type.id == 1, date, NA),
           implementationdate = ifelse(date.type.id == 2, date, NA),
           removaldate = ifelse(date.type.id == 3, date, NA)) %>%
    select(!c(date, date.type.id)) %>%
    mutate(hints = hint.id) %>%
    group_by(hints) %>%
    nest() %>%
    mutate_at(.vars = 'data', .funs = function(x){ 
              x %>% 
                map(.f = function(x1) {
                  x1 %>%
                    as.list() %>%
                    map(.f = function(x) { 
                                x = ifelse(lengths(x) > 0, discard(x,is.na), x)
                                return(unique(x))
                              }) %>%
                    map_at(.at = 'relevance', .f = function(x){
                      relevance = ifelse(!is.null(hint.relevance), hint.relevance, x)
                        return(relevance)
                    }) %>%
                    map_at(.at = 'assessment.id', .f = function(x){
                      assessment.name = mapvalues(x, assessment.list$assessment.id, assessment.list$assessment.name, warn_missing = F) 
                      assessment.name <- assessment.name %>%
                        discard(.p = assessment.name %in% remove.assessment) %>%
                        append(add.assessment)
                      assessment.name[length(assessment.name) == 0] <- NA
                      return(unique(assessment.name))
                    }) %>%
                    map_at(.at = 'intervention.id', .f = function(x) { 
                      intervention.name = mapvalues(x, intervention.list$intervention.type.id, intervention.list$intervention.type.name, warn_missing = F)
                      intervention.name <- intervention.name %>%
                        discard(.p = intervention.name %in% remove.instrument) %>%
                        append(add.instrument)
                      intervention.name[length(intervention.name) == 0] <- NA
                      return(unique(intervention.name))
                    }) %>%
                    map_at(.at = 'product.group.id', .f = function(x) { 
                      product.name = mapvalues(x, product.list$product.group.id, product.list$product.group.name, warn_missing = F)
                      product.name <- product.name %>%
                        discard(.p = product.name %in% remove.product) %>%
                        append(add.product)
                      product.name[length(product.name) == 0] <- NA
                      return(unique(product.name))
                    }) %>%
                    map_at(.at = 'jurisdiction.id', .f = function(x) { 
                      jurisdiction.name = mapvalues(x, jurisdiction.list$jurisdiction.id, jurisdiction.list$jurisdiction.name, warn_missing = F)
                      jurisdiction.name <- jurisdiction.name %>%
                        discard(.p = jurisdiction.name %in% remove.jurisdiction) %>%
                        append(add.jurisdiction)
                      jurisdiction.name[length(jurisdiction.name) == 0] <- NA
                      return(unique(jurisdiction.name))
                    }) %>%
                    map_at(.at = 'announcementdate', .f = function(x) { 
                      announcementdate = x
                      announcementdate = ifelse(!is.null(remove.date.announced) & announcementdate %in% remove.date.announced, NA, announcementdate)
                      announcementdate = ifelse(!is.null(add.date.announced), add.date.announced, announcementdate)
                      return(announcementdate)
                    }) %>%
                    map_at(.at = 'implementationdate', .f = function(x) { 
                      implementationdate = x
                      implementationdate = ifelse(!is.null(remove.date.implemented) & implementationdate %in% remove.date.implemented, NA, implementationdate)
                      implementationdate = ifelse(!is.null(add.date.implemented), add.date.implemented, implementationdate)
                      return(implementationdate)
                    }) %>%
                    map_at(.at = 'removaldate', .f = function(x) { 
                      removaldate = x
                      removaldate = ifelse(!is.null(remove.date.removed) & removaldate %in% remove.date.removed, NA, removaldate)
                      removaldate = ifelse(!is.null(add.date.removed), add.date.removed, removaldate)
                      return(removaldate)
                    })
                })
      })

  test_pass.attributes <<- pass.hint.attributes
  # if hint is part of a collection the collection functions need to instead be ran!
  if(nrow(col.id) == 0){ #is.na(col.id)
    ## hint is not part of a collection
    
    # names as an input in the b221_process_display_info
    out_names <- c('id','clicked','country','product','intervention','assessment','url','official','comment',
                   'implementationdate','announcementdate','removaldate', 'discard_reasons', 'discard_comment')


  } else {
    ## hint is part of a collection
    col.star=gta_sql_get_value(paste0("SELECT hint_id FROM b221_collection_star WHERE collection_id = ",col.id," ;"))
    
    if(col.star!=change.id){stop("The hint you are changing is part of a collection but not its star. Make it the star or change the star.")}
    
  }
  
  
}