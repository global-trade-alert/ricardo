library(pool)
library(gtasql)
library(gtalibrary)
library(plyr)
library(tidyverse)
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


b221_hint_change_attribute<-function(change.id=NULL,
                                     is.intervention=F,
                                     intervention.modifiable=F,
                                     modify.assessment=NULL,
                                     modify.date.announced=NULL,
                                     modify.date.implemented=NULL,
                                     modify.date.removed=NULL,
                                     modify.relevance=NULL,
                                     modify.title=NULL,
                                     modify.description=NULL,
                                     add.instrument=NULL,
                                     remove.instrument=NULL,
                                     add.product=NULL,
                                     remove.product=NULL,
                                     add.jurisdiction=NULL,
                                     remove.jurisdiction=NULL){
  
  # stop if intervention is not allowed to be modified
  if(is.intervention & !intervention.modifiable) stop('Changing attributes of interventions is not allowed.')
  
  # find hint id if intervention id is what is provided
  if(is.intervention){
    change.id=unique(gta_sql_get_value(paste0("SELECT DISTINCT hint_id FROM bt_hint_log WHERE gta_id IN (",paste0(change.id, collapse = ','),");")))
    change.id=change.id[!is.na(change.id)]
    if(length(change.id)<1) return("no such intervention id was found")
  }
  
  col.id=gta_sql_get_value(paste0("SELECT DISTINCT hint_id, collection_id FROM b221_hint_collection WHERE hint_id IN (",paste0(change.id, collapse=','),");")) #paste0("SELECT DISTINCT hint_id, collection_id FROM b221_hint_collection WHERE hint_id = ",change.id,";")
  test_col.id <<- col.id
  
  # find attributes pre-change
  pull.hint.attributes = sprintf(paste0("	SELECT DISTINCT change_ids.hint_id, ht_ass.assessment_id, ht_int.apparent_intervention_id AS intervention_id, 
                                        	prod_grp.product_group_id, ht_jur.jurisdiction_id, ht_rlvnt.relevance, bt_hint_date.`date`, bt_hint_date.date_type_id,
                                        	bt_hint_url.url_id, bt_hint_url.url_type_id, bt_hint_text.hint_title, bt_hint_text.hint_description ,
                                        	bt_hint_discard_reason.discard_reason_id, bt_hint_discard_reason.discard_reason_comment, b221_hint_comment_log.comment FROM 
                                        	(SELECT bt_hint_log.hint_id FROM bt_hint_log WHERE bt_hint_log.hint_id IN (%s)) change_ids 
                                        	LEFT JOIN (SELECT b221_hint_assessment.hint_id, b221_hint_assessment.assessment_id, b221_hint_assessment.assessment_accepted FROM b221_hint_assessment JOIN (SELECT b221_hint_assessment.hint_id, MAX(b221_hint_assessment.validation_classification) AS newest_classification FROM b221_hint_assessment GROUP BY hint_id) newest_classification ON newest_classification.hint_id = b221_hint_assessment.hint_id AND newest_classification.newest_classification <=> b221_hint_assessment.validation_classification) ht_ass ON ht_ass.hint_id = change_ids.hint_id AND ht_ass.assessment_accepted = 1
                                        	LEFT JOIN (SELECT b221_hint_intervention.hint_id, b221_hint_intervention.apparent_intervention_id, b221_hint_intervention.intervention_accepted FROM b221_hint_intervention JOIN (SELECT b221_hint_intervention.hint_id, MAX(b221_hint_intervention.validation_classification) AS newest_classification FROM b221_hint_intervention GROUP BY hint_id) newest_classification ON newest_classification.hint_id = b221_hint_intervention.hint_id AND newest_classification.newest_classification <=> b221_hint_intervention.validation_classification) ht_int ON ht_int.hint_id = change_ids.hint_id AND ht_int.intervention_accepted = 1
                                        	LEFT JOIN (SELECT b221_hint_product_group.hint_id, b221_hint_product_group.product_group_id, b221_hint_product_group.product_group_assessment FROM b221_hint_product_group JOIN (SELECT b221_hint_product_group.hint_id, MAX(b221_hint_product_group.validation_classification) AS newest_classification FROM b221_hint_product_group GROUP BY hint_id) newest_classification ON newest_classification.hint_id = b221_hint_product_group.hint_id AND newest_classification.newest_classification <=> b221_hint_product_group.validation_classification) prod_grp ON prod_grp.hint_id = change_ids.hint_id AND prod_grp.product_group_assessment = 1
                                        	LEFT JOIN (SELECT bt_hint_jurisdiction.hint_id, bt_hint_jurisdiction.jurisdiction_id, bt_hint_jurisdiction.jurisdiction_accepted FROM bt_hint_jurisdiction JOIN (SELECT bt_hint_jurisdiction.hint_id, MAX(bt_hint_jurisdiction.validation_classification) AS newest_classification FROM bt_hint_jurisdiction GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_jurisdiction.hint_id AND newest_classification.newest_classification <=> bt_hint_jurisdiction.validation_classification) ht_jur ON ht_jur.hint_id = change_ids.hint_id AND ht_jur.jurisdiction_accepted = 1
                                        	LEFT JOIN (SELECT bt_hint_relevance.hint_id, bt_hint_relevance.relevance, bt_hint_relevance.relevance_accepted FROM bt_hint_relevance JOIN (SELECT bt_hint_relevance.hint_id, MAX(bt_hint_relevance.validation_classification) AS newest_classification FROM bt_hint_relevance GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_relevance.hint_id AND newest_classification.newest_classification <=> bt_hint_relevance.validation_classification) ht_rlvnt ON ht_rlvnt.hint_id = change_ids.hint_id AND ht_rlvnt.relevance_accepted = 1
                                        	LEFT JOIN (SELECT bt_hint_date.hint_id, bt_hint_date.`date`, bt_hint_date.date_type_id, bt_hint_date.date_accepted FROM bt_hint_date JOIN (SELECT bt_hint_date.hint_id, MAX(bt_hint_date.validation_classification) AS newest_classification FROM bt_hint_date GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_date.hint_id AND newest_classification.newest_classification <=> bt_hint_date.validation_classification) bt_hint_date ON bt_hint_date.hint_id = change_ids.hint_id AND bt_hint_date.date_accepted = 1
                                        	LEFT JOIN (SELECT bt_hint_url.hint_id, bt_hint_url.url_id , bt_hint_url.url_type_id , bt_hint_url.url_accepted FROM bt_hint_url JOIN (SELECT bt_hint_url.hint_id, MAX(bt_hint_url.validation_classification ) AS newest_classification FROM bt_hint_url GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_url.hint_id AND newest_classification.newest_classification <=> bt_hint_url.validation_classification) bt_hint_url ON bt_hint_url.hint_id = change_ids.hint_id AND bt_hint_url.url_accepted = 1
                                        	LEFT JOIN (SELECT bt_hint_text.hint_id, bt_hint_text.hint_title , bt_hint_text.hint_description , bt_hint_text.description_accepted FROM bt_hint_text JOIN (SELECT bt_hint_text.hint_id, MAX(bt_hint_text.validation_classification ) AS newest_classification FROM bt_hint_text GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_text.hint_id AND newest_classification.newest_classification <=> bt_hint_text.validation_classification) bt_hint_text ON bt_hint_text.hint_id = change_ids.hint_id AND bt_hint_text.description_accepted = 1
                                        	LEFT JOIN (SELECT bt_hint_discard_reason.hint_id, bt_hint_discard_reason.discard_reason_id, bt_hint_discard_reason.discard_reason_comment, bt_hint_discard_reason.reason_accepted FROM bt_hint_discard_reason JOIN (SELECT bt_hint_discard_reason.hint_id, MAX(bt_hint_discard_reason.validation_classification ) AS newest_classification FROM bt_hint_discard_reason GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_discard_reason.hint_id AND newest_classification.newest_classification <=> bt_hint_discard_reason.validation_classification) bt_hint_discard_reason ON bt_hint_discard_reason.hint_id = change_ids.hint_id AND bt_hint_discard_reason.reason_accepted = 1
                                        	LEFT JOIN b221_hint_comment_log ON change_ids.hint_id = b221_hint_comment_log.hint_id;"),ifelse(paste0(change.id, collapse = ',')=='',"NULL",paste0(change.id, collapse = ',')))
  pull.hint.attributes = gta_sql_get_value(pull.hint.attributes)

  #pull.hint.attributes = merge(pull.hint.attributes, col.id, by = 'hint.id')

  #add dfs with id and names of the attributes
  assessment.list <- gta_sql_get_value(paste0("SELECT DISTINCT bal.assessment_id , bal.assessment_name FROM b221_assessment_list bal WHERE bal.assessment_id IN(",paste0(unique(ifelse(is.na(pull.hint.attributes$assessment.id), 'NULL',pull.hint.attributes$assessment.id )), collapse=','),");"))
  intervention.list <- gta_sql_get_value(paste0("SELECT DISTINCT bitl.intervention_type_id, bitl.intervention_type_name FROM b221_intervention_type_list bitl WHERE bitl.intervention_type_id IN (", paste0(unique(ifelse(is.na(pull.hint.attributes$intervention.id), 'NULL',pull.hint.attributes$intervention.id )),collapse=',') ,");"))
  product.list <- gta_sql_get_value(paste0("SELECT DISTINCT bpgl.product_group_id, bpgl.product_group_name FROM b221_product_group_list bpgl WHERE bpgl.product_group_id IN (", paste0(unique(ifelse(is.na(pull.hint.attributes$product.group.id), 'NULL',pull.hint.attributes$product.group.id )),collapse=',') ,");"))
  jurisdiction.list <- gta_sql_get_value(paste0("SELECT DISTINCT gjl.jurisdiction_id, gjl.jurisdiction_name FROM gta_jurisdiction_list gjl WHERE gjl.jurisdiction_id IN (", paste0(unique(ifelse(is.na(pull.hint.attributes$jurisdiction.id), 'NULL',pull.hint.attributes$jurisdiction.id)),collapse=',') ,");"))
  discard.reasons.list <- gta_sql_get_value(paste0("SELECT DISTINCT bdrl.discard_reason_id, bdrl.discard_reason_name FROM bt_discard_reason_list bdrl WHERE bdrl.discard_reason_id IN (", paste0(unique(ifelse(is.na(pull.hint.attributes$discard.reason.id), 'NULL',pull.hint.attributes$discard.reason.id)),collapse=',') ,");"))
  url.list <- gta_sql_get_value(paste0("SELECT DISTINCT url_id, url FROM bt_url_log WHERE url_id IN (", paste0(unique(ifelse(is.na(pull.hint.attributes$url.id), 'NULL',pull.hint.attributes$url.id)),collapse=',') ,");"))
  url.type <- gta_sql_get_value(paste0("SELECT DISTINCT url_type_id, url_type_name FROM bt_url_type_list WHERE url_type_id IN (", paste0(unique(ifelse(is.na(pull.hint.attributes$url.type.id), 'NULL',pull.hint.attributes$url.type.id)),collapse=',') ,");"))
  date.type <- gta_sql_get_value(paste0("SELECT DISTINCT date_type_id, date_type_name FROM bt_date_type_list WHERE date_type_id IN (", paste0(unique(ifelse(is.na(pull.hint.attributes$date.type.id), 'NULL',pull.hint.attributes$date.type.id)),collapse=',') ,");"))

  test_pull.attributes <<- pull.hint.attributes

  pull.hint.attributes <- pull.hint.attributes %>% 
                              left_join(col.id, by = 'hint.id')
  
  # COMPARE EXISTENT VALUES WITH FEEDED ----------------------------------------------------------
  pass.hint.attributes <-
    pull.hint.attributes %>%
    mutate_at(.vars='date.type.id', .funs = function(x) {
      mapvalues(unlist(x), date.type$date.type.id, date.type$date.type.name, warn_missing = F)
    }) %>%
    mutate(is.official = NA_character_,
           announcementdate = ifelse(date.type.id == 'announcement', date, NA_character_),
           implementationdate = ifelse(date.type.id == 'implementation', date, NA_character_),
           removaldate = ifelse(date.type.id == 'remove', date, NA_character_)) %>%
    mutate_at(.vars='url.type.id', .funs = function(x) {
      url.type = mapvalues(unlist(x), url.type$url.type.id, url.type$url.type.name, warn_missing = F)
      return(unlist(url.type))
    }) %>%
    mutate_at(.vars = 'url.id', .funs = function(x) {
      url = mapvalues(unlist(x), url.list$url.id, url.list$url, warn_missing = F)
      return(unlist(url))
    }) %>%
    mutate(is.official = ifelse(url.type.id == 'official', 1, 0),
           comment = unlist(comment)) %>%
    select(!c(date, date.type.id, url.type.id)) %>%
    # Change attributes according to feeded into the function--------------------
    rowwise() %>%
    mutate_at(.vars = 'hint.title', .funs = function(x){
      hint.title = ifelse(is.null(modify.title), x, modify.title)
      return(hint.title)
    }) %>%
    mutate_at(.vars = 'hint.description', .funs = function(x){
      hint.description = ifelse(is.null(modify.description), x, modify.description)
      return(hint.description)
    }) %>%
    mutate_at(.vars = 'assessment.id', .funs = function(x){
      assessment.name = mapvalues(x, assessment.list$assessment.id, assessment.list$assessment.name, warn_missing = F)
      assessment.name = ifelse(is.null(modify.assessment), assessment.name, modify.assessment)
      return(assessment.name)
    }) %>%
    mutate_at(.vars = 'intervention.id', .funs = function(x) {
      intervention.name = mapvalues(x, intervention.list$intervention.type.id, intervention.list$intervention.type.name, warn_missing = F)
      intervention.name = ifelse(intervention.name %in% remove.instrument, NA_character_, intervention.name)
      return(intervention.name)
    }) %>%
    mutate_at(.vars = 'product.group.id', .funs = function(x) {
        product.name = mapvalues(x, product.list$product.group.id, product.list$product.group.name, warn_missing = F)
        product.name = ifelse(product.name %in% remove.product, NA_character_, product.name)
        return(product.name)
    }) %>%
    mutate_at(.vars = 'jurisdiction.id', .funs = function(x) {
        jurisdiction.name = mapvalues(x, jurisdiction.list$jurisdiction.id, jurisdiction.list$jurisdiction.name, warn_missing = F)
        jurisdiction.name = ifelse(jurisdiction.name %in% remove.jurisdiction, NA_character_, jurisdiction.name)
        return(jurisdiction.name)
    }) %>%
    mutate_at(.vars = 'announcementdate', .funs = function(x) {
        announcementdate = ifelse(is.null(modify.date.announced), x, modify.date.announced)
        return(announcementdate)
    }) %>%
    mutate_at(.vars = 'implementationdate', .funs = function(x) {
        implementationdate = ifelse(is.null(modify.date.implemented), x, modify.date.implemented)
        return(implementationdate)
    }) %>%
    mutate_at(.vars = 'removaldate', .funs = function(x) {
        removaldate = ifelse(is.null(modify.date.removed), x, modify.date.removed)
        return(removaldate)
    }) %>%
    mutate_at(.vars = 'relevance', .funs = function(x) {
      relevance = ifelse(!is.null(modify.relevance), modify.relevance, NA_character_)
      return(relevance)
    }) %>%
    mutate(hints = hint.id) %>%
    group_by(hints) %>%
    nest() %>%
    map(as.list) %>%
    pluck('data') %>%
    #make lists for fields with multiple values permitted ----------------------
    map(.f = function(x){
      x %>%
        modify(.f = function(x){
          x <- x %>%
            unique %>%
            na.omit
          if(length(x) == 0) x = NA_character_
          if(length(x) > 1) x = list(x)
          return(x)
        }) %>%
        filter(row_number() == 1) %>%
          modify_at(.at = 'intervention.id', .f = function(x){
            intervention.name <- x %>%
              unlist %>%
              append(add.instrument) %>%
              unique
            intervention.name = intervention.name[!is.na(intervention.name)]
            if(length(intervention.name) == 0) intervention.name = NA_character_
            return(list(intervention.name))
          }) %>%
         modify_at(.at = 'product.group.id', .f = function(x){
            product.name <- x %>%
              unlist %>%
              append(add.product) %>%
              unique
            product.name = product.name[!is.na(product.name)]
            if(length(product.name) == 0) product.name = NA_character_
            return(list(product.name))
          }) %>%
         modify_at(.at = 'jurisdiction.id', .f = function(x){
            jurisdiction.name <- x %>%
              unlist %>%
              append(add.jurisdiction) %>%
              unique
            jurisdiction.name = jurisdiction.name[!is.na(jurisdiction.name)]
            if(length(jurisdiction.name) == 0) jurisdiction.name = NA_character_
            return(list(jurisdiction.name))
          }) %>%
          modify_at(.at = 'discard.reason.id', .f = function(x){
            discard.name <- x
            return(list(discard.name))
          })
    })
  
  test_pass.attributes <<- pass.hint.attributes
  
  # validate if we are not feeding empty attributes to b221_process_display/b221_process_collections
  pass.hint.attributes %>%
    map(.f = function(x){
      hint.id = x$hint.id
      x %>%
        mutate_at(.vars = c('jurisdiction.id', 'product.group.id', 'intervention.id', 'assessment.id', 'relevance'),
                  .funs = function(x){
                    colname = quo_name(enquo(x))
                    val = unlist(x)
                    if(length(val) == 1)
                      if(is.na(val)) stop(paste0("Hint ",hint.id," contains empty attributes in field '", colname, "'"))
                  })
    })

  # run b221_process_display_info or b221_process_collections for each row of pass.hint.attributes
  pass.hint.attributes %>%
        map(.f = function(x){
            if(is.na(x$collection.id)){
              ## hint is not part of a collection ------------------------------
                  output <- x %>% 
                    select(hint.id, relevance, jurisdiction.id, product.group.id, intervention.id, assessment.id, url.id, is.official, comment, implementationdate, announcementdate, removaldate,
                           hint.title, hint.description, discard.reason.id, discard.reason.comment) %>%
                    setnames(c('id','clicked','country','product','intervention','assessment','url','official','comment',
                               'implementationdate','announcementdate','removaldate', 'title', 'hint.description', 'discard.reason', 'discard.reason.comment'))
                  test_ht <<- output
                  
                  b221_process_display_info(is.freelancer = FALSE ,user.id = 82, processed.rows = output, text.modifiable = TRUE)

            } else {
              ## hint is part of a collection ----------------------------------
              output <- x
              test_cl <<- output
              col.star=gta_sql_get_value(paste0("SELECT hint_id FROM b221_collection_star WHERE collection_id = ",unlist(output$collection.id)," ;"))

              if(col.star!=output$hint.id){stop("The hint you are changing is part of a collection but not its star. Make it the star or change the star.")}
              
              #add dfs with id and names of the changed attributes
              assessment.list <- gta_sql_get_value(paste0("SELECT DISTINCT bal.assessment_id , bal.assessment_name FROM b221_assessment_list bal WHERE bal.assessment_name = '",paste0(ifelse(is.na(output$assessment.id), 'NULL',output$assessment.id )),"';"))
              intervention.list <- gta_sql_get_value(paste0("SELECT DISTINCT bitl.intervention_type_id, bitl.intervention_type_name FROM b221_intervention_type_list bitl WHERE bitl.intervention_type_name IN ('", paste0(unique(ifelse(is.na(unlist(output$intervention.id)), 'NULL',unlist(output$intervention.id ))),collapse="','") ,"');"))
              product.list <- gta_sql_get_value(paste0("SELECT DISTINCT bpgl.product_group_id, bpgl.product_group_name FROM b221_product_group_list bpgl WHERE bpgl.product_group_name IN ('", paste0(unique(ifelse(is.na(unlist(output$product.group.id)), 'NULL',unlist(output$product.group.id ))),collapse="','") ,"');"))
              jurisdiction.list <- gta_sql_get_value(paste0("SELECT DISTINCT gjl.jurisdiction_id, gjl.jurisdiction_name FROM gta_jurisdiction_list gjl WHERE gjl.jurisdiction_name IN ('", paste0(unique(ifelse(is.na(unlist(output$jurisdiction.id)), 'NULL',unlist(output$jurisdiction.id))),collapse="','") ,"');"))
              discard.reasons.list <- gta_sql_get_value(paste0("SELECT DISTINCT bdrl.discard_reason_id, bdrl.discard_reason_name FROM bt_discard_reason_list bdrl WHERE bdrl.discard_reason_name IN ('", paste0(unique(ifelse(is.na(unlist(output$discard.reason.id)), 'NULL',unlist(output$discard.reason.id))),collapse="','") ,"');"))
               
              #map values name -> id
              ImplementerId = as.numeric(mapvalues(unlist(output$jurisdiction.id), jurisdiction.list$jurisdiction.name, jurisdiction.list$jurisdiction.id, warn_missing = F))
              InterventionTypeId = as.numeric(mapvalues(unlist(output$intervention.id), intervention.list$intervention.type.name, intervention.list$intervention.type.id, warn_missing = F))
              ProductId = as.numeric(mapvalues(unlist(output$product.group.id), product.list$product.group.name, product.list$product.group.id, warn_missing = F))
              AssessmentId = as.numeric(mapvalues(output$assessment.id, assessment.list$assessment.name, assessment.list$assessment.id, warn_missing = F))
              DiscardReasonsId = as.numeric(mapvalues(unlist(output$discard.reason.id), discard.reasons.list$discard.reason.name, discard.reasons.list$discard.reason.id, warn_missing = F))
              DiscardReasonsId = ifelse(is.na(DiscardReasonsId), NA, DiscardReasonsId)
              DiscardComment = ifelse(is.na(output$discard.reason.comment), "", discard.reason.comment)
              
              ImplementerId <<- ImplementerId
              InterventionTypeId <<- InterventionTypeId
              ProductId <<- ProductId
              AssessmentId <<- AssessmentId
              DiscardReasonsId <<- DiscardReasonsId
              DiscardComment <<- DiscardComment
              
              attributes = bt_find_collection_attributes(collection.id = output$collection.id, 
                                                         hints.id = output$hint.id, 
                                                         starred.hint.id = col.star, 
                                                         country = ImplementerId, 
                                                         product = ProductId, 
                                                         intervention = InterventionTypeId, 
                                                         assessment = AssessmentId, 
                                                         relevance = output$relevance, 
                                                         #discard = DiscardReasonsId,
                                                         #discard.comment = DiscardComment,
                                                         announcement.date = output$announcementdate, 
                                                         implementation.date = output$implementationdate, 
                                                         removal.date = output$removaldate)
              
              test_attributes <<- attributes
              
              b221_process_collections_hints(is.freelancer = F,
                                             user.id = 82,
                                             collection.id = output$collection.id,
                                             hints.id = output$hint.id,
                                             starred.hint.id = attributes$starred.hint.id,
                                             country = attributes$country,
                                             product = attributes$product,
                                             intervention = attributes$intervention,
                                             assessment = attributes$assessment,
                                             #discard = attributes$discard,
                                             #discard.comment = attributes$discard.comment,
                                             announcement.date = attributes$announcement.date,
                                             implementation.date = attributes$implementation.date,
                                             removal.date = attributes$removal.date,
                                             relevance = output$relevance,
                                             collection.unchanged = attributes$collection.unchanged,
                                             empty.attributes = F)
            }
        })
}