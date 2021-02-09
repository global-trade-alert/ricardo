b221_user_accuracy_rating = function(time.period = ""){
  
  #set up connection
  library(gtasql)
  
  source("setup/keys/ric.R")
  # pool = pool::dbPool(
  #   drv = RMySQL::MySQL(),
  #   host = db.host,
  #   username = db.user,
  #   password = db.password,
  #   dbname=db.name
  # )
  # rm(db.host, db.user, db.password, db.name)
  session.prefix="bt_"
  
  if(nchar(time.period)>0){
    time.period = paste0("AND btcl.time_stamp > (select NOW() - INTERVAL ", time.period, " DAY)")
  }
  
  user.accuracy.rating = gta_sql_get_value(query = paste0(
  "SELECT btcl.user_id, SUM(bthr.relevance_accepted)/COUNT(bthr.relevance_accepted) AS user_accuracy
  FROM bt_hint_relevance bthr, bt_classification_log btcl
  WHERE bthr.classification_id = btcl.classification_id
  AND btcl.hint_state_id IN (1, 2)
  AND bthr.hint_id IN (
  	SELECT bthrcl2.hint_id 
  	FROM  (
  			SELECT bthr.hint_id, bthr.classification_id
  			FROM bt_hint_relevance bthr, bt_classification_log btcl
  			WHERE bthr.classification_id = btcl.classification_id
  			AND btcl.hint_state_id IN (1, 2)
  			) bthrcl2
  )", time.period, "
  GROUP BY btcl.user_id")
  )
  
  
  return(user.accuracy.rating)
  
}
