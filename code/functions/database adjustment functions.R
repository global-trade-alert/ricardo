


## HINT level
b221_hint_change_attribute<-function(change.id=NULL,
                                     is.intervention=F,
                                     is.state.act=F,
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
  
  if(is.intervention){
    change.id=unique(b221_convert_intervention(change.id)$hint.id)
  }
  
  
  if(is.state.act){
    change.id=unique(b221_convert_state_act(change.id)$hint.id)
  }
  
  
  col.id=gta_sql_get_value(paste0("SELECT DISTINCT collection_id FROM b221_hint_collection WHERE hint_id = ",change.id,";"))
  
  if(is.na(col.id)){
  ## hint is not part of a collection
    
    
  } else {
  ## hint is part of a collection
    col.star=gta_sql_get_value(paste0("SELECT hint_id FROM b221_collection_star WHERE collection_id = ",col.id," ;"))
    
    if(col.star!=change.id){stop("The hint you are changing is part of a collection but not its star. Make it the star or change the star.")}
    
  }
  
  
}


## COLLECTION level

## add hints to collection
b221_collection_add_hint<-function(add.id=NULL,
                                   is.intervention=F,
                                   is.state.act=F,
                                   add.star=NULL,
                                   star.is.intervention=F,
                                   star.is.state.act=F,
                                   col.id=NULL,
                                   col.name=NULL){
  
  if(! is.null(col.name) & ! is.null(col.id)){
    stop("Please only supply either the collection ID or it's name.")
  }
  
  if(is.null(col.name) & is.null(col.id)){
    stop("Please supply either the collection ID or it's name.")
  }
  
  if(! is.null(col.name)){
    col.id=gta_sql_get_value(paste0("SELECT collection_id
                                     FROM b221_collection_log
                                     WHERE collection_name = '",col.name,";"))
    
    if(is.na(col.id)){stop("Unknown collection name. Please correct or provide ID.")}
  }
  
  if(is.intervention){
    add.id=unique(b221_convert_intervention(add.id)$hint.id)
  }
  
  if(is.state.act){
    add.id=unique(b221_convert_state_act(add.id)$hint.id)
  }
  
  
  if(! is.null(add.star)){
    
    col.star=add.star
    
    
    
  } else {
    
    if(any(star.is.intervention)){
      
      col.star=unique(c(add.star[! star.is.intervention],
                        unique(b221_convert_intervention(add.star[star.is.intervention])$hint.id)))
    }
    
    if(any(star.is.state.act)){
      
      col.star=unique(c(add.star[! star.is.state.act],
                        unique(b221_convert_state.act(add.star[star.is.state.act])$hint.id)))
    }
    
    col.star=gta_sql_get_value(paste0("SELECT DISTINCT hint_id 
                                       FROM b221_collection_star
                                       WHERE collection_id IN (",paste(add.star, collapse=","),");"))
  }
  
  col.star=col.star[! is.na(col.star)]
  
  if(length(col.star)==0){stop("There is no valid starred hint in this collection.")}
  
  
  col.country=gta_sql_get_value(paste0("SELECT DISTINCT jurisdiction_id FROM bt_hint_jurisdiction WHERE hint_id IN (",paste(col.star, collapse=','),") AND jurisdiction_accepted=1;"))
  col.ass=gta_sql_get_value(paste0("SELECT DISTINCT assessment_id FROM b221_hint_assessment WHERE hint_id IN (",paste(col.star, collapse=','),") AND assessment_accepted=1;"))
  col.int=gta_sql_get_value(paste0("SELECT DISTINCT apparent_intervention_id FROM b221_hint_intervention WHERE hint_id IN (",paste(col.star, collapse=','),") AND intervention_accepted=1;"))
  col.prd=gta_sql_get_value(paste0("SELECT DISTINCT product_group_id FROM b221_hint_product_group WHERE hint_id IN (",paste(col.star, collapse=','),") AND product_group_assessment=1;"))
  col.rel=gta_sql_get_value(paste0("SELECT DISTINCT relevance FROM bt_hint_relevance WHERE hint_id IN (",paste(col.star, collapse=','),") AND relevance_accepted=1;"))
  
  col.announced=gta_sql_get_value(paste0("SELECT DISTINCT date FROM bt_hint_date WHERE hint_id IN (",paste(col.star, collapse=','),") AND date_type_id=1 AND date_accepted=1;"))
  col.implemented=gta_sql_get_value(paste0("SELECT DISTINCT date FROM bt_hint_date WHERE hint_id IN (",paste(col.star, collapse=','),") AND date_type_id=2 AND date_accepted=1;"))
  col.removed=gta_sql_get_value(paste0("SELECT DISTINCT date FROM bt_hint_date WHERE hint_id IN (",paste(col.star, collapse=','),") AND date_type_id=3 AND date_accepted=1;"))
  
  
  if(any(is.na(c(col.country, col.ass, col.int, col.prd, col.rel)))){
    stop(paste("Your starred hint misses validate entries for: ",
               c("country. ","assessment. ","instrument. ","product. ","relevance. ")[which(is.na(c(col.country, col.ass, col.int, col.prd, col.rel)))]))
  }
  
  col.hints=unique(c(add.id, 
                     gta_sql_get_value(paste0("SELECT DISTINCT hint_id 
                                       FROM b221_hint_collection
                                       WHERE collection_id IN (",paste(add.star, collapse=","),");"))))
  
  


  attributes = bt_find_collection_attributes(collection.id = col.id,
                                             hints.id = col.hints, 
                                             starred.hint.id = col.star, 
                                             country = col.country, 
                                             product = col.prd, 
                                             intervention = col.int, 
                                             assessment = col.ass, 
                                             relevance = col.rel, 
                                             announcement.date = col.announced, 
                                             implementation.date = col.implemented, 
                                             removal.date = col.removed)
  
  b221_process_collections_hints(is.freelancer = FALSE,
                                 user.id = 1,
                                 collection.id = col.id,
                                 hints.id = col.hints,
                                 country = attributes$country,
                                 product = attributes$product,
                                 intervention = attributes$intervention,
                                 assessment = attributes$assessment,
                                 relevance = attributes$relevance,
                                 announcement.date = attributes$announcement.date,
                                 implementation.date = attributes$implementation.date,
                                 removal.date = attributes$removal.date,
                                 collection.unchanged = attributes$collection.unchanged,
                                 starred.hint.id = attributes$starred.hint.id,
                                 empty.attributes = F)
   rm('attributes')
  
  
  
}

## remove collection via it's ID
b221_collection_remove<-function(remove.collection.id=NULL){
  
  
  gta_sql_multiple_queries(paste0("DELETE FROM b221_hint_collection WHERE collection_id IN (",paste(remove.collection.id, collapse = ","),");
                                 DELETE FROM b221_collection_assessment WHERE collection_id IN (",paste(remove.collection.id, collapse = ","),");
                                DELETE FROM b221_collection_date WHERE collection_id IN (",paste(remove.collection.id, collapse = ","),");
                                DELETE FROM b221_collection_intervention WHERE collection_id IN (",paste(remove.collection.id, collapse = ","),");
                                DELETE FROM b221_collection_jurisdiction WHERE collection_id IN (",paste(remove.collection.id, collapse = ","),");
                                DELETE FROM b221_collection_product_group WHERE collection_id IN (",paste(remove.collection.id, collapse = ","),");
                                DELETE FROM b221_collection_relevance WHERE collection_id IN (",paste(remove.collection.id, collapse = ","),");
                                DELETE FROM b221_collection_star WHERE collection_id IN (",paste(remove.collection.id, collapse = ","),");
                                DELETE FROM b221_collection_log WHERE collection_id IN (",paste(remove.collection.id, collapse = ","),");"),1)
  
  
}




## hint/intervention/state act ID conversion functions

b221_convert_intervention<-function(find.intervention=NULL){
  
  hint.intervention=gta_sql_get_value(paste0("SELECT hint_id, gta.id as intervention_id
                                              FROM bt_hint_log
                                              WHERE gta_id IN (",paste(find.intervention, collapse=","),");"))
  
  
  return(hint.intervention) 
}


b221_convert_state_act<-function(find.state.act=NULL){
  
  database <<- "gtamain"
  
  gta_sql_pool_open(pool.name="main",
                    db.title=database,
                    db.host = gta_pwd(database)$host,
                    db.name = gta_pwd(database)$name,
                    db.user = gta_pwd(database)$user,
                    db.password = gta_pwd(database)$password,
                    table.prefix = "bt_")
  
  intervention.state.act=gta_sql_get_value(paste0("SELECT id as intervention_id, measure_id as state_act_id
                                           FROM gta_intervention
                                           WHERE id IN (",paste(find.state.act, collapse=","),");"),
                                           "main")
  gta_pool_close("main")
  
  
  hint.intervention=b221_convert_intervention(unique(intervention.state.act$intervention.id))
  
  hint.state.act=merge(intervention.state.act, hint.intervention, by="intervention.id", all.x=T)
  
  return(hint.state.act)
  
  
}
