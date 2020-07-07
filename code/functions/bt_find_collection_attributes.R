bt_find_collection_attributes=function(new.collection.name = NULL, collection.id = NULL, hints.id = NULL, starred.hint.id = NULL, country = NULL,
                                       product = NULL, intervention = NULL, assessment = NULL, relevance = NULL, discard = NULL, announcement.date = NULL, implementation.date = NULL, removal.date = NULL){
  
  hints.id = unique(hints.id)
  hints.id=hints.id[!is.na(hints.id)]
  
  pull.intervention.attributes = sprintf(paste0("SELECT gta_ids.hint_id, b221_hint_assessment.assessment_id, b221_hint_intervention.apparent_intervention_id AS intervention_id, 
                                                      b221_hint_product_group.product_group_id, bt_hint_jurisdiction.jurisdiction_id, bt_hint_relevance.relevance, bt_hint_discard_reason.discard_reason_id,
                                                      bt_hint_date.`date`, bt_hint_date.date_type_id FROM 
                                                      (SELECT bt_hint_log.hint_id FROM bt_hint_log WHERE bt_hint_log.hint_id IN (%s) AND bt_hint_log.gta_id IS NOT NULL) gta_ids 
                                                      LEFT JOIN b221_hint_assessment ON b221_hint_assessment.hint_id = gta_ids.hint_id AND b221_hint_assessment.assessment_accepted = 1
                                                      LEFT JOIN b221_hint_intervention ON b221_hint_intervention.hint_id = gta_ids.hint_id AND b221_hint_intervention.intervention_accepted = 1
                                                      LEFT JOIN b221_hint_product_group ON b221_hint_product_group.hint_id = gta_ids.hint_id AND b221_hint_product_group.product_group_assessment = 1
                                                      LEFT JOIN bt_hint_jurisdiction ON bt_hint_jurisdiction.hint_id = gta_ids.hint_id AND bt_hint_jurisdiction.jurisdiction_accepted = 1
                                                      LEFT JOIN bt_hint_relevance ON bt_hint_relevance.hint_id = gta_ids.hint_id AND bt_hint_relevance.relevance_accepted = 1
                                                      LEFT JOIN bt_hint_discard_reason ON bt_hint_discard_reason.hint_id = gta_ids.hint_id AND bt_hint_discard_reason.reason_accepted = 1
                                                      LEFT JOIN bt_hint_date ON bt_hint_date.hint_id = gta_ids.hint_id AND bt_hint_date.date_accepted = 1;"),ifelse(paste0(hints.id, collapse = ',')=='',"NULL",paste0(hints.id, collapse = ',')))
  pull.intervention.attributes = gta_sql_get_value(pull.intervention.attributes)
  pull_attr <<- pull.intervention.attributes
  
  # if there are interventions in the collection, these supercede the provided attributes
  if(nrow(pull.intervention.attributes)>0){
    intervention = na.omit(unique(pull.intervention.attributes$intervention.id))
    product = na.omit(unique(pull.intervention.attributes$product.group.id))
    country = na.omit(unique(pull.intervention.attributes$jurisdiction.id))
    announcement.date = na.omit(min(subset(pull.intervention.attributes, date.type.id == 1)$date))
    implementation.date = na.omit(min(subset(pull.intervention.attributes, date.type.id == 2)$date))
    removal.date = na.omit(max(subset(tidyr::spread(pull.intervention.attributes, key='date.type.id', value='date'), !is.na(`2`))$`3`))
    assessment = na.omit(ifelse(length(unique(pull.intervention.attributes$assessment.id))>1,4,unique(pull.intervention.attributes$assessment.id)))
    relevance = 1
    discard = na.omit(unique(pull.intervention.attributes$discard.reason.id))
    starred.hint.id = unique(pull.intervention.attributes$hint.id) # if there is an intervention in a collection then this should be starred (can non intervention hints be starred whilst in a collection with an intervention? I answer no here)
  }
  print(discard)
  # check if collection's attributes were changed
  if(!is.null(collection.id)){
    
    query = paste0("SELECT DISTINCT cltn_log.collection_id, cltn_log.collection_name, cltn_jur.jurisdiction_id, cltn_ass.assessment_id, cltn_int.intervention_type_id, cltn_prod.product_group_id,cltn_rel.relevance,
                          cltn_dis.discard_reason_id,
                          IF(bt_date_type_list.date_type_name='announcement', col_date.date, NULL ) AS announcement_date,
                          IF(bt_date_type_list.date_type_name='implementation', col_date.date, NULL ) AS implementation_date,
                          IF(bt_date_type_list.date_type_name='removal', col_date.date, NULL ) AS removal_date
                          FROM b221_collection_log cltn_log
                          JOIN b221_collection_jurisdiction cltn_jur ON cltn_jur.collection_id = cltn_log.collection_id AND cltn_log.collection_id = ",collection.id," 
                          JOIN b221_collection_assessment cltn_ass ON cltn_ass.collection_id = cltn_log.collection_id 
                          JOIN b221_collection_intervention cltn_int ON cltn_int.collection_id = cltn_log.collection_id 
                          JOIN b221_collection_product_group cltn_prod ON cltn_prod.collection_id = cltn_log.collection_id 
                          JOIN b221_collection_relevance cltn_rel ON cltn_rel.collection_id = cltn_log.collection_id
                          JOIN b221_collection_discard_reasons cltn_dis ON cltn_dis.collection_id = cltn_log.collection_id
                          LEFT JOIN b221_collection_date col_date ON col_date.collection_id = cltn_log.collection_id LEFT JOIN bt_date_type_list ON col_date.date_type_id = bt_date_type_list.date_type_id;
                         ")
    collectionStats <- gta_sql_get_value(query)
    
    # compare old and new values of collection to determine whether b221_process_collections() should cascade or not
    if (nrow(collectionStats)>0) {
      if (length(c(
        setdiff(union(country, collectionStats$jurisdiction.id),intersect(country, collectionStats$jurisdiction.id)),
        setdiff(union(intervention, collectionStats$intervention.type.id),intersect(intervention, collectionStats$intervention.type.id)),
        setdiff(union(assessment, collectionStats$assessment.id),intersect(assessment, collectionStats$assessment.id)),
        setdiff(union(product, collectionStats$product.group.id),intersect(product, collectionStats$product.group.id)),
        setdiff(union(discard, collectionStats$discard.reason.id),intersect(discard, collectionStats$discard.reason.id)),
        setdiff(union(as.character(announcement.date), as.character(na.omit(collectionStats$announcement.date))), intersect(as.character(announcement.date), as.character(na.omit(collectionStats$announcement.date)))),
        setdiff(union(as.character(implementation.date), as.character(na.omit(collectionStats$implementation.date))), intersect(as.character(implementation.date), as.character(na.omit(collectionStats$implementation.date)))),
        setdiff(union(as.character(removal.date), as.character(na.omit(collectionStats$removal.date))), intersect(as.character(removal.date), as.character(na.omit(collectionStats$removal.date))))
      )) > 0 ) {
        collectionChanged = T
      } else {
        collectionChanged = F
      }
    } else {
      collectionChanged = T # Collection must have changed in this scenario, otherwise it could not be saved
    }
  } else {
    collectionChanged = T
  }
  
  return(list(collection.unchanged = !collectionChanged, starred.hint.id = starred.hint.id, country = country, product = product, intervention = intervention, assessment = assessment, relevance = relevance, discard = discard, announcement.date = announcement.date, implementation.date = implementation.date, removal.date = removal.date))
  
}



