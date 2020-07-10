bt_delete_collection=function(collection.id=NULL){
  if(is.null(collection.id) | length(collection.id)<1) return('your collection id is null/empty for some reason')
  
  collection.id = as.numeric(collection.id[!is.na(collection.id)])
  
  if(any(is.na(collection.id))) return('non numerically interpretable collection ids!')
  
  collections.sql = paste0("SELECT ",collection.id[1]," AS collection_id")
  if(length(collection.id[-1])>0) collections.sql = paste0(collections.sql, ' UNION SELECT ' , paste0(collection.id[-1], collapse = ' UNION SELECT '))
  
  del.statement = sprintf(paste0("DELETE b221_collection_assessment, b221_collection_date, b221_collection_intervention, b221_collection_jurisdiction, b221_collection_product_group, b221_collection_relevance, b221_collection_star, b221_hint_collection, b221_collection_log, b221_collection_discard_reasons
                                  FROM (",collections.sql,") del_collections
                                  LEFT JOIN b221_collection_assessment ON b221_collection_assessment.collection_id = del_collections.collection_id
                                  LEFT JOIN b221_collection_date ON b221_collection_date.collection_id = del_collections.collection_id
                                  LEFT JOIN b221_collection_intervention ON b221_collection_intervention.collection_id = del_collections.collection_id
                                  LEFT JOIN b221_collection_jurisdiction ON b221_collection_jurisdiction.collection_id = del_collections.collection_id
                                  LEFT JOIN b221_collection_product_group ON b221_collection_product_group.collection_id = del_collections.collection_id
                                  LEFT JOIN b221_collection_relevance ON b221_collection_relevance.collection_id = del_collections.collection_id
                                  LEFT JOIN b221_collection_discard_reasons ON b221_collection_discard_reasons.collection_id = del_collections.collection_id
                                  LEFT JOIN b221_collection_star ON b221_collection_star.collection_id = del_collections.collection_id
                                  LEFT JOIN b221_hint_collection ON b221_hint_collection.collection_id = del_collections.collection_id
                                  LEFT JOIN b221_collection_log ON b221_collection_log.collection_id = del_collections.collection_id
                                  WHERE del_collections.collection_id IN (%s);"), paste0(collection.id, collapse = ','))
  
  gta_sql_get_value(del.statement)
  
  return('successful')
  
}