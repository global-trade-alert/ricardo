bt_evaluate_user_performance=function(return.xlsx = F, xlsx.path = NULL){
  
  library(gtasql)
  library(gtalibrary)
  library(pool)
  
  gta_setwd()
  possible.statistics = c('processed_hints','validated_hints','state_two_hint_counts','accuracy_measures_relevance','accuracy_measures_jurisdiction','accuracy_measures_instrument','accuracy_measures_assessment','accuracy_measures_product','accuracy_measures_date')
  sql.call = paste0("CALL compute_user_performance_stats('",possible.statistics,"')")
  
  result = list()
  for(i in 1:length(possible.statistics)){
    database <- "ricardomain"
    gta_sql_pool_open(db.title=database,db.host = gta_pwd(database)$host,db.name = gta_pwd(database)$name,db.user = gta_pwd(database)$user,db.password = gta_pwd(database)$password,table.prefix = "")
    result[[i]] = gta_sql_get_value(sql.call[i])
    gta_sql_pool_close()
  }
  names(result) <- possible.statistics
  if(return.xlsx==T & !is.null(xlsx.path)) openxlsx::write.xlsx(result, paste0(xlsx.path,Sys.Date(),' ricardo database statistics.xlsx')) else return(result)
}

# /* The following is an SQL script which should be ran in the database and stores a function inside the database 
# * I leave the procedure (equivalent of a function but in SQL) here in case further modifications are intended later on 
# * gtasql has no possibility of running an actual script in sql yet and thus the reason why i cannot execute this script via R 
# * usage example is "CALL compute_user_performance_stats(stat_type)" with stat_type taking a single string value
# * possible string values: 'processed_hints' 'accuracy_measures_relevance' 'accuracy_measures_jurisdiction' 'accuracy_measures_instrument' 'accuracy_measures_assessment' 'accuracy_measures_product' 'accuracy_measures_date' */
#   
# DELIMITER $$
#   $$
#   /* PROCEDURE TO ALLOW QUERIES FOR USER PERFORMANCE */
#   CREATE DEFINER=`gtaricardomaster`@`%` PROCEDURE `ricardo`.`compute_user_performance_stats`(stat_type TINYTEXT)
# BEGIN
# CREATE TEMPORARY TABLE `__increment_days` (`min_date` DATETIME NOT NULL,`max_date` DATETIME NOT NULL);
# SET @dt = (SELECT CONCAT(DATE((MIN(time_stamp)-INTERVAL WEEKDAY(MIN(time_stamp)) +3 DAY)), ' ','23:59:59') FROM bt_classification_log);
# INSERT INTO `__increment_days` VALUES (@dt, @dt + INTERVAL 7 DAY);
# WHILE (@dt := @dt + INTERVAL 7 DAY) < CURRENT_DATE() DO
# INSERT INTO `__increment_days` VALUES (@dt, @dt + INTERVAL 7 DAY);
# END WHILE;
# IF stat_type = 'processed_hints' THEN
# SELECT min_date, max_date, gta_user_log.f_name, gta_user_log.l_name, bt_hint_state_list.hint_state_name, COUNT(DISTINCT(bt_hint_relevance.hint_id)) AS counts FROM `__increment_days`
# CROSS JOIN bt_hint_relevance
# JOIN bt_classification_log ON bt_hint_relevance.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date 
# JOIN (SELECT gta_user_log.user_id, gta_user_log.f_name, gta_user_log.l_name FROM gta_user_log WHERE NOT EXISTS (SELECT NULL FROM gta_user_group WHERE gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id <3)) gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id
# JOIN bt_hint_log ON bt_hint_relevance.hint_id = bt_hint_log.hint_id JOIN bt_hint_state_list ON bt_hint_log.hint_state_id = bt_hint_state_list.hint_state_id
# GROUP BY min_date, max_date, gta_user_log.user_id, bt_hint_log.hint_state_id;
# END IF;
# IF stat_type = 'validated_hints' THEN
# SELECT min_date, max_date, gta_user_log.f_name, gta_user_log.l_name, bt_hint_state_list.hint_state_name, COUNT(DISTINCT(bt_hint_relevance.hint_id)) AS counts FROM `__increment_days`
# CROSS JOIN bt_hint_relevance
# JOIN bt_classification_log ON bt_hint_relevance.validation_classification = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date 
# JOIN (SELECT gta_user_log.user_id, gta_user_log.f_name, gta_user_log.l_name FROM gta_user_log WHERE EXISTS (SELECT NULL FROM gta_user_group WHERE gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id <3)) gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id
# JOIN bt_hint_log ON bt_hint_relevance.hint_id = bt_hint_log.hint_id JOIN bt_hint_state_list ON bt_hint_log.hint_state_id = bt_hint_state_list.hint_state_id
# GROUP BY min_date, max_date, gta_user_log.user_id, bt_hint_log.hint_state_id;
# END IF;
# IF stat_type = 'state_two_hint_counts' THEN 
# SELECT COUNT(*) AS count_state_two_hints, int_list.intervention_type_name AS 'parameter_value', 'instrument' AS parameter_name FROM bt_hint_log
# LEFT JOIN (SELECT b221_hint_intervention.hint_id, b221_hint_intervention.apparent_intervention_id, b221_hint_intervention.intervention_accepted FROM b221_hint_intervention 
#            JOIN (SELECT b221_hint_intervention.hint_id, MAX(b221_hint_intervention.classification_id) AS newest_proposition, MAX(b221_hint_intervention.validation_classification) AS newest_validation 
#                  FROM b221_hint_intervention GROUP BY hint_id) newest_classification 
#            ON newest_classification.hint_id = b221_hint_intervention.hint_id AND IF(newest_classification.newest_validation IS NOT NULL,newest_classification.newest_validation,newest_classification.newest_proposition) = IF(newest_classification.newest_validation IS NOT NULL,b221_hint_intervention.validation_classification,b221_hint_intervention.classification_id)) ht_int 
# ON ht_int.hint_id = bt_hint_log.hint_id AND (ht_int.intervention_accepted = 1 OR ht_int.intervention_accepted IS NULL) 
# LEFT JOIN b221_intervention_type_list int_list ON int_list.intervention_type_id = ht_int.apparent_intervention_id
# WHERE hint_state_id = 2
# GROUP BY int_list.intervention_type_name
# UNION
# SELECT COUNT(*) AS count_state_two_hints, prod_list.product_group_name AS 'parameter_value', 'product' AS parameter_name FROM bt_hint_log
# LEFT JOIN (SELECT b221_hint_product_group.hint_id, b221_hint_product_group.product_group_id, b221_hint_product_group.product_group_assessment FROM b221_hint_product_group 
#            JOIN (SELECT b221_hint_product_group.hint_id, MAX(b221_hint_product_group.classification_id) AS newest_proposition, MAX(b221_hint_product_group.validation_classification) AS newest_validation 
#                  FROM b221_hint_product_group GROUP BY hint_id) newest_classification ON newest_classification.hint_id = b221_hint_product_group.hint_id AND IF(newest_classification.newest_validation IS NOT NULL,newest_classification.newest_validation,newest_classification.newest_proposition) = IF(newest_classification.newest_validation IS NOT NULL,b221_hint_product_group.validation_classification,b221_hint_product_group.classification_id)) ht_prod_grp 
# ON ht_prod_grp.hint_id = bt_hint_log.hint_id AND (ht_prod_grp.product_group_assessment = 1 OR ht_prod_grp.product_group_assessment IS NULL) 
# LEFT JOIN b221_product_group_list prod_list ON prod_list.product_group_id = ht_prod_grp.product_group_id
# WHERE hint_state_id = 2
# GROUP BY prod_list.product_group_name;
# END IF;
# IF stat_type = 'accuracy_measures_relevance' THEN
# SELECT result_matrix.*, N_matrix.N FROM
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.f_name, distinct_entries.l_name, distinct_entries.user_id, GROUP_CONCAT(distinct_entries.hint_id ORDER BY distinct_entries.hint_id ASC) AS processed_hints, distinct_entries.relevance, distinct_entries.relevance_accepted, COUNT(distinct_entries.hint_id) AS n
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.f_name, highest_classification_in_window.l_name, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, distinct_attributes.relevance, distinct_attributes.relevance_accepted
#     FROM
#     (SELECT DISTINCT min_date, max_date, bt_hint_relevance.hint_id, gta_user_log.f_name, gta_user_log.l_name, gta_user_log.user_id, MAX(bt_hint_relevance.classification_id) AS classification_id, bt_hint_relevance.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN bt_hint_relevance 
#       JOIN bt_classification_log ON bt_hint_relevance.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE relevance_accepted IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, bt_hint_relevance.hint_id) highest_classification_in_window
#     JOIN (SELECT bt_hint_relevance.hint_id, bt_hint_relevance.classification_id, relevance, relevance_accepted FROM bt_hint_relevance JOIN bt_classification_log ON bt_classification_log.classification_id = bt_hint_relevance.classification_id AND bt_hint_relevance.relevance_accepted IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id, distinct_entries.relevance, distinct_entries.relevance_accepted) result_matrix
# JOIN 
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id, GROUP_CONCAT(distinct_entries.hint_id ORDER BY distinct_entries.hint_id ASC) AS processed_hints, distinct_entries.relevance, distinct_entries.relevance_accepted, COUNT(distinct_entries.hint_id) AS N
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, distinct_attributes.relevance, distinct_attributes.relevance_accepted
#     FROM
#     (SELECT DISTINCT min_date, max_date, bt_hint_relevance.hint_id, gta_user_log.user_id, MAX(bt_hint_relevance.classification_id) AS classification_id, bt_hint_relevance.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN bt_hint_relevance 
#       JOIN bt_classification_log ON bt_hint_relevance.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE relevance_accepted IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, bt_hint_relevance.hint_id) highest_classification_in_window
#     JOIN (SELECT bt_hint_relevance.hint_id, bt_hint_relevance.classification_id, relevance, relevance_accepted FROM bt_hint_relevance JOIN bt_classification_log ON bt_classification_log.classification_id = bt_hint_relevance.classification_id AND bt_hint_relevance.relevance_accepted IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id) N_matrix 
# ON result_matrix.min_date = N_matrix.min_date AND result_matrix.max_date = N_matrix.max_date AND result_matrix.user_id = N_matrix.user_id;
# END IF;
# IF stat_type = 'accuracy_measures_jurisdiction' THEN
# SELECT result_matrix.*, N_matrix.N, proposition_matrix.processed_hints, proposition_matrix.wrong_country, proposition_matrix.correct_country FROM
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.f_name, distinct_entries.l_name, distinct_entries.user_id, distinct_entries.jurisdiction_accepted, COUNT(distinct_entries.hint_id) AS n
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.f_name, highest_classification_in_window.l_name, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, distinct_attributes.jurisdiction_id, distinct_attributes.jurisdiction_accepted
#     FROM
#     (SELECT DISTINCT min_date, max_date, bt_hint_jurisdiction.hint_id, gta_user_log.f_name, gta_user_log.l_name, gta_user_log.user_id, MAX(bt_hint_jurisdiction.classification_id) AS classification_id, bt_hint_jurisdiction.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN bt_hint_jurisdiction 
#       JOIN bt_classification_log ON bt_hint_jurisdiction.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE jurisdiction_accepted IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, bt_hint_jurisdiction.hint_id) highest_classification_in_window
#     JOIN (SELECT bt_hint_jurisdiction.hint_id, bt_hint_jurisdiction.classification_id, jurisdiction_id, jurisdiction_accepted FROM bt_hint_jurisdiction JOIN bt_classification_log ON bt_classification_log.classification_id = bt_hint_jurisdiction.classification_id AND bt_hint_jurisdiction.jurisdiction_accepted IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id, distinct_entries.jurisdiction_accepted) result_matrix
# JOIN 
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.f_name, distinct_entries.l_name, distinct_entries.user_id, GROUP_CONCAT(distinct_entries.hint_id ORDER BY distinct_entries.hint_id ASC) AS processed_hints, distinct_entries.jurisdiction_accepted, COUNT(distinct_entries.hint_id) AS n
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.f_name, highest_classification_in_window.l_name, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, distinct_attributes.jurisdiction_id, distinct_attributes.jurisdiction_accepted
#     FROM
#     (SELECT DISTINCT min_date, max_date, bt_hint_jurisdiction.hint_id, gta_user_log.f_name, gta_user_log.l_name, gta_user_log.user_id, MAX(bt_hint_jurisdiction.classification_id) AS classification_id, bt_hint_jurisdiction.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN bt_hint_jurisdiction 
#       JOIN bt_classification_log ON bt_hint_jurisdiction.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE jurisdiction_accepted IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, bt_hint_jurisdiction.hint_id) highest_classification_in_window
#     JOIN (SELECT bt_hint_jurisdiction.hint_id, bt_hint_jurisdiction.classification_id, jurisdiction_id, jurisdiction_accepted FROM bt_hint_jurisdiction JOIN bt_classification_log ON bt_classification_log.classification_id = bt_hint_jurisdiction.classification_id AND bt_hint_jurisdiction.jurisdiction_accepted IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id) N_matrix 
# ON result_matrix.min_date = N_matrix.min_date AND result_matrix.max_date = N_matrix.max_date AND result_matrix.user_id = N_matrix.user_id
# JOIN 
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.f_name, distinct_entries.l_name, distinct_entries.user_id, GROUP_CONCAT(distinct_entries.hint_id ORDER BY distinct_entries.hint_id ASC) AS processed_hints, distinct_entries.jurisdiction_accepted, GROUP_CONCAT(IF(distinct_entries.jurisdiction_accepted = 0, distinct_entries.jurisdiction_name, NULL) ORDER BY distinct_entries.hint_id ASC) AS wrong_country, 
#   GROUP_CONCAT((CASE WHEN distinct_entries.jurisdiction_accepted = 0 AND distinct_entries.correct_country IS NULL THEN '-' WHEN distinct_entries.jurisdiction_accepted = 0 THEN distinct_entries.correct_country ELSE NULL END) ORDER BY distinct_entries.hint_id ASC) AS correct_country
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.f_name, highest_classification_in_window.l_name, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, gta_jurisdiction_list.jurisdiction_name, distinct_attributes.jurisdiction_accepted, correct_country.jurisdiction_name AS correct_country 
#     FROM
#     (SELECT DISTINCT min_date, max_date, bt_hint_jurisdiction.hint_id, gta_user_log.f_name, gta_user_log.l_name, gta_user_log.user_id, MAX(bt_hint_jurisdiction.classification_id) AS classification_id, bt_hint_jurisdiction.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN bt_hint_jurisdiction 
#       JOIN bt_classification_log ON bt_hint_jurisdiction.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE jurisdiction_accepted IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, bt_hint_jurisdiction.hint_id) highest_classification_in_window
#     JOIN (SELECT bt_hint_jurisdiction.hint_id, bt_hint_jurisdiction.classification_id, bt_hint_jurisdiction.validation_classification, jurisdiction_id, jurisdiction_accepted FROM bt_hint_jurisdiction JOIN bt_classification_log ON bt_classification_log.classification_id = bt_hint_jurisdiction.classification_id AND bt_hint_jurisdiction.jurisdiction_accepted IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id
#     JOIN gta_jurisdiction_list ON gta_jurisdiction_list.jurisdiction_id = distinct_attributes.jurisdiction_id
#     LEFT JOIN bt_hint_jurisdiction correct_classification ON correct_classification.hint_id = distinct_attributes.hint_id AND distinct_attributes.jurisdiction_accepted = 0 AND correct_classification.jurisdiction_accepted = 1 AND distinct_attributes.validation_classification = correct_classification.validation_classification LEFT JOIN gta_jurisdiction_list correct_country ON correct_country.jurisdiction_id = correct_classification.jurisdiction_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id, distinct_entries.jurisdiction_accepted) proposition_matrix
# ON result_matrix.min_date = proposition_matrix.min_date AND result_matrix.max_date = proposition_matrix.max_date AND result_matrix.user_id = proposition_matrix.user_id AND result_matrix.jurisdiction_accepted = proposition_matrix.jurisdiction_accepted;
# END IF;
# IF stat_type = 'accuracy_measures_instrument' THEN
# SELECT result_matrix.*, N_matrix.N, proposition_matrix.processed_hints, proposition_matrix.wrong_instrument, proposition_matrix.correct_instrument FROM
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.f_name, distinct_entries.l_name, distinct_entries.user_id, distinct_entries.intervention_accepted, COUNT(distinct_entries.hint_id) AS n
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.f_name, highest_classification_in_window.l_name, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, distinct_attributes.apparent_intervention_id, distinct_attributes.intervention_accepted
#     FROM
#     (SELECT DISTINCT min_date, max_date, b221_hint_intervention.hint_id, gta_user_log.f_name, gta_user_log.l_name, gta_user_log.user_id, MAX(b221_hint_intervention.classification_id) AS classification_id, b221_hint_intervention.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN b221_hint_intervention 
#       JOIN bt_classification_log ON b221_hint_intervention.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE intervention_accepted IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, b221_hint_intervention.hint_id) highest_classification_in_window
#     JOIN (SELECT b221_hint_intervention.hint_id, b221_hint_intervention.classification_id, apparent_intervention_id, intervention_accepted FROM b221_hint_intervention JOIN bt_classification_log ON bt_classification_log.classification_id = b221_hint_intervention.classification_id AND b221_hint_intervention.intervention_accepted IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id, distinct_entries.intervention_accepted) result_matrix
# JOIN 
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.f_name, distinct_entries.l_name, distinct_entries.user_id, GROUP_CONCAT(distinct_entries.hint_id ORDER BY distinct_entries.hint_id ASC) AS processed_hints, distinct_entries.intervention_accepted, COUNT(distinct_entries.hint_id) AS n
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.f_name, highest_classification_in_window.l_name, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, distinct_attributes.apparent_intervention_id, distinct_attributes.intervention_accepted
#     FROM
#     (SELECT DISTINCT min_date, max_date, b221_hint_intervention.hint_id, gta_user_log.f_name, gta_user_log.l_name, gta_user_log.user_id, MAX(b221_hint_intervention.classification_id) AS classification_id, b221_hint_intervention.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN b221_hint_intervention 
#       JOIN bt_classification_log ON b221_hint_intervention.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE intervention_accepted IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, b221_hint_intervention.hint_id) highest_classification_in_window
#     JOIN (SELECT b221_hint_intervention.hint_id, b221_hint_intervention.classification_id, apparent_intervention_id, intervention_accepted FROM b221_hint_intervention JOIN bt_classification_log ON bt_classification_log.classification_id = b221_hint_intervention.classification_id AND b221_hint_intervention.intervention_accepted IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id) N_matrix 
# ON result_matrix.min_date = N_matrix.min_date AND result_matrix.max_date = N_matrix.max_date AND result_matrix.user_id = N_matrix.user_id
# JOIN 
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.f_name, distinct_entries.l_name, distinct_entries.user_id, GROUP_CONCAT(distinct_entries.hint_id ORDER BY distinct_entries.hint_id ASC) AS processed_hints, distinct_entries.intervention_accepted, GROUP_CONCAT(IF(distinct_entries.intervention_accepted = 0, distinct_entries.intervention_type_name, NULL) ORDER BY distinct_entries.hint_id ASC) AS wrong_instrument, 
#   GROUP_CONCAT((CASE WHEN distinct_entries.intervention_accepted = 0 AND distinct_entries.correct_instrument IS NULL THEN '-' WHEN distinct_entries.intervention_accepted = 0 THEN distinct_entries.correct_instrument ELSE NULL END) ORDER BY distinct_entries.hint_id ASC) AS correct_instrument
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.f_name, highest_classification_in_window.l_name, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, b221_intervention_type_list.intervention_type_name, distinct_attributes.intervention_accepted, correct_instrument.intervention_type_name AS correct_instrument 
#     FROM
#     (SELECT DISTINCT min_date, max_date, b221_hint_intervention.hint_id, gta_user_log.f_name, gta_user_log.l_name, gta_user_log.user_id, MAX(b221_hint_intervention.classification_id) AS classification_id, b221_hint_intervention.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN b221_hint_intervention 
#       JOIN bt_classification_log ON b221_hint_intervention.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE intervention_accepted IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, b221_hint_intervention.hint_id) highest_classification_in_window
#     JOIN (SELECT b221_hint_intervention.hint_id, b221_hint_intervention.classification_id, b221_hint_intervention.validation_classification, apparent_intervention_id, intervention_accepted FROM b221_hint_intervention JOIN bt_classification_log ON bt_classification_log.classification_id = b221_hint_intervention.classification_id AND b221_hint_intervention.intervention_accepted IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id
#     JOIN b221_intervention_type_list ON b221_intervention_type_list.intervention_type_id = distinct_attributes.apparent_intervention_id
#     LEFT JOIN b221_hint_intervention correct_classification ON correct_classification.hint_id = distinct_attributes.hint_id AND distinct_attributes.intervention_accepted = 0 AND correct_classification.intervention_accepted = 1 AND distinct_attributes.validation_classification = correct_classification.validation_classification LEFT JOIN b221_intervention_type_list correct_instrument ON correct_instrument.intervention_type_id = correct_classification.apparent_intervention_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id, distinct_entries.intervention_accepted) proposition_matrix
# ON result_matrix.min_date = proposition_matrix.min_date AND result_matrix.max_date = proposition_matrix.max_date AND result_matrix.user_id = proposition_matrix.user_id AND result_matrix.intervention_accepted = proposition_matrix.intervention_accepted;
# END IF;
# IF stat_type = 'accuracy_measures_assessment' THEN
# SELECT result_matrix.*, N_matrix.N, proposition_matrix.processed_hints, proposition_matrix.wrong_assessment, proposition_matrix.correct_assessment FROM
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.f_name, distinct_entries.l_name, distinct_entries.user_id, distinct_entries.assessment_accepted, COUNT(distinct_entries.hint_id) AS n
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.f_name, highest_classification_in_window.l_name, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, distinct_attributes.assessment_id, distinct_attributes.assessment_accepted
#     FROM
#     (SELECT DISTINCT min_date, max_date, b221_hint_assessment.hint_id, gta_user_log.f_name, gta_user_log.l_name, gta_user_log.user_id, MAX(b221_hint_assessment.classification_id) AS classification_id, b221_hint_assessment.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN b221_hint_assessment 
#       JOIN bt_classification_log ON b221_hint_assessment.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE assessment_accepted IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, b221_hint_assessment.hint_id) highest_classification_in_window
#     JOIN (SELECT b221_hint_assessment.hint_id, b221_hint_assessment.classification_id, assessment_id, assessment_accepted FROM b221_hint_assessment JOIN bt_classification_log ON bt_classification_log.classification_id = b221_hint_assessment.classification_id AND b221_hint_assessment.assessment_accepted IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id, distinct_entries.assessment_accepted) result_matrix
# JOIN 
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.f_name, distinct_entries.l_name, distinct_entries.user_id, GROUP_CONCAT(distinct_entries.hint_id ORDER BY distinct_entries.hint_id ASC) AS processed_hints, distinct_entries.assessment_accepted, COUNT(distinct_entries.hint_id) AS n
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.f_name, highest_classification_in_window.l_name, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, distinct_attributes.assessment_id, distinct_attributes.assessment_accepted
#     FROM
#     (SELECT DISTINCT min_date, max_date, b221_hint_assessment.hint_id, gta_user_log.f_name, gta_user_log.l_name, gta_user_log.user_id, MAX(b221_hint_assessment.classification_id) AS classification_id, b221_hint_assessment.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN b221_hint_assessment 
#       JOIN bt_classification_log ON b221_hint_assessment.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE assessment_accepted IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, b221_hint_assessment.hint_id) highest_classification_in_window
#     JOIN (SELECT b221_hint_assessment.hint_id, b221_hint_assessment.classification_id, assessment_id, assessment_accepted FROM b221_hint_assessment JOIN bt_classification_log ON bt_classification_log.classification_id = b221_hint_assessment.classification_id AND b221_hint_assessment.assessment_accepted IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id) N_matrix 
# ON result_matrix.min_date = N_matrix.min_date AND result_matrix.max_date = N_matrix.max_date AND result_matrix.user_id = N_matrix.user_id
# JOIN 
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.f_name, distinct_entries.l_name, distinct_entries.user_id, GROUP_CONCAT(distinct_entries.hint_id ORDER BY distinct_entries.hint_id ASC) AS processed_hints, distinct_entries.assessment_accepted, GROUP_CONCAT(IF(distinct_entries.assessment_accepted = 0, distinct_entries.assessment_name, NULL) ORDER BY distinct_entries.hint_id ASC) AS wrong_assessment, 
#   GROUP_CONCAT((CASE WHEN distinct_entries.assessment_accepted = 0 AND distinct_entries.correct_assessment IS NULL THEN '-' WHEN distinct_entries.assessment_accepted = 0 THEN distinct_entries.correct_assessment ELSE NULL END) ORDER BY distinct_entries.hint_id ASC) AS correct_assessment
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.f_name, highest_classification_in_window.l_name, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, b221_assessment_list.assessment_name, distinct_attributes.assessment_accepted, correct_assessment.assessment_name AS correct_assessment 
#     FROM
#     (SELECT DISTINCT min_date, max_date, b221_hint_assessment.hint_id, gta_user_log.f_name, gta_user_log.l_name, gta_user_log.user_id, MAX(b221_hint_assessment.classification_id) AS classification_id, b221_hint_assessment.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN b221_hint_assessment 
#       JOIN bt_classification_log ON b221_hint_assessment.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE assessment_accepted IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, b221_hint_assessment.hint_id) highest_classification_in_window
#     JOIN (SELECT b221_hint_assessment.hint_id, b221_hint_assessment.classification_id, b221_hint_assessment.validation_classification, assessment_id, assessment_accepted FROM b221_hint_assessment JOIN bt_classification_log ON bt_classification_log.classification_id = b221_hint_assessment.classification_id AND b221_hint_assessment.assessment_accepted IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id
#     JOIN b221_assessment_list ON b221_assessment_list.assessment_id = distinct_attributes.assessment_id
#     LEFT JOIN b221_hint_assessment correct_classification ON correct_classification.hint_id = distinct_attributes.hint_id AND distinct_attributes.assessment_accepted = 0 AND correct_classification.assessment_accepted = 1 AND distinct_attributes.validation_classification = correct_classification.validation_classification LEFT JOIN b221_assessment_list correct_assessment ON correct_assessment.assessment_id = correct_classification.assessment_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id, distinct_entries.assessment_accepted) proposition_matrix
# ON result_matrix.min_date = proposition_matrix.min_date AND result_matrix.max_date = proposition_matrix.max_date AND result_matrix.user_id = proposition_matrix.user_id AND result_matrix.assessment_accepted = proposition_matrix.assessment_accepted;
# END IF;
# IF stat_type = 'accuracy_measures_product' THEN
# SELECT result_matrix.*, N_matrix.N, proposition_matrix.processed_hints, proposition_matrix.wrong_product, proposition_matrix.correct_product FROM
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.f_name, distinct_entries.l_name, distinct_entries.user_id, distinct_entries.product_group_assessment, COUNT(distinct_entries.hint_id) AS n
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.f_name, highest_classification_in_window.l_name, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, distinct_attributes.product_group_id, distinct_attributes.product_group_assessment
#     FROM
#     (SELECT DISTINCT min_date, max_date, b221_hint_product_group.hint_id, gta_user_log.f_name, gta_user_log.l_name, gta_user_log.user_id, MAX(b221_hint_product_group.classification_id) AS classification_id, b221_hint_product_group.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN b221_hint_product_group 
#       JOIN bt_classification_log ON b221_hint_product_group.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE product_group_assessment IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, b221_hint_product_group.hint_id) highest_classification_in_window
#     JOIN (SELECT b221_hint_product_group.hint_id, b221_hint_product_group.classification_id, product_group_id, product_group_assessment FROM b221_hint_product_group JOIN bt_classification_log ON bt_classification_log.classification_id = b221_hint_product_group.classification_id AND b221_hint_product_group.product_group_assessment IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id, distinct_entries.product_group_assessment) result_matrix
# JOIN 
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.f_name, distinct_entries.l_name, distinct_entries.user_id, GROUP_CONCAT(distinct_entries.hint_id ORDER BY distinct_entries.hint_id ASC) AS processed_hints, distinct_entries.product_group_assessment, COUNT(distinct_entries.hint_id) AS n
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.f_name, highest_classification_in_window.l_name, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, distinct_attributes.product_group_id, distinct_attributes.product_group_assessment
#     FROM
#     (SELECT DISTINCT min_date, max_date, b221_hint_product_group.hint_id, gta_user_log.f_name, gta_user_log.l_name, gta_user_log.user_id, MAX(b221_hint_product_group.classification_id) AS classification_id, b221_hint_product_group.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN b221_hint_product_group 
#       JOIN bt_classification_log ON b221_hint_product_group.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE product_group_assessment IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, b221_hint_product_group.hint_id) highest_classification_in_window
#     JOIN (SELECT b221_hint_product_group.hint_id, b221_hint_product_group.classification_id, product_group_id, product_group_assessment FROM b221_hint_product_group JOIN bt_classification_log ON bt_classification_log.classification_id = b221_hint_product_group.classification_id AND b221_hint_product_group.product_group_assessment IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id) N_matrix 
# ON result_matrix.min_date = N_matrix.min_date AND result_matrix.max_date = N_matrix.max_date AND result_matrix.user_id = N_matrix.user_id
# JOIN 
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.f_name, distinct_entries.l_name, distinct_entries.user_id, GROUP_CONCAT(distinct_entries.hint_id ORDER BY distinct_entries.hint_id ASC) AS processed_hints, distinct_entries.product_group_assessment, GROUP_CONCAT(IF(distinct_entries.product_group_assessment = 0, distinct_entries.product_group_name, NULL) ORDER BY distinct_entries.hint_id ASC) AS wrong_product, 
#   GROUP_CONCAT((CASE WHEN distinct_entries.product_group_assessment = 0 AND distinct_entries.correct_product IS NULL THEN '-' WHEN distinct_entries.product_group_assessment = 0 THEN distinct_entries.correct_product ELSE NULL END) ORDER BY distinct_entries.hint_id ASC) AS correct_product
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.f_name, highest_classification_in_window.l_name, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, b221_product_group_list.product_group_name, distinct_attributes.product_group_assessment, correct_product.product_group_name AS correct_product 
#     FROM
#     (SELECT DISTINCT min_date, max_date, b221_hint_product_group.hint_id, gta_user_log.f_name, gta_user_log.l_name, gta_user_log.user_id, MAX(b221_hint_product_group.classification_id) AS classification_id, b221_hint_product_group.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN b221_hint_product_group 
#       JOIN bt_classification_log ON b221_hint_product_group.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE product_group_assessment IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, b221_hint_product_group.hint_id) highest_classification_in_window
#     JOIN (SELECT b221_hint_product_group.hint_id, b221_hint_product_group.classification_id, b221_hint_product_group.validation_classification, product_group_id, product_group_assessment FROM b221_hint_product_group JOIN bt_classification_log ON bt_classification_log.classification_id = b221_hint_product_group.classification_id AND b221_hint_product_group.product_group_assessment IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id
#     JOIN b221_product_group_list ON b221_product_group_list.product_group_id = distinct_attributes.product_group_id
#     LEFT JOIN b221_hint_product_group correct_classification ON correct_classification.hint_id = distinct_attributes.hint_id AND distinct_attributes.product_group_assessment = 0 AND correct_classification.product_group_assessment = 1 AND distinct_attributes.validation_classification = correct_classification.validation_classification LEFT JOIN b221_product_group_list correct_product ON correct_product.product_group_id = correct_classification.product_group_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id, distinct_entries.product_group_assessment) proposition_matrix
# ON result_matrix.min_date = proposition_matrix.min_date AND result_matrix.max_date = proposition_matrix.max_date AND result_matrix.user_id = proposition_matrix.user_id AND result_matrix.product_group_assessment = proposition_matrix.product_group_assessment;
# END IF;
# IF stat_type = 'accuracy_measures_date' THEN
# SELECT result_matrix.*, N_matrix.N FROM
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.f_name, distinct_entries.l_name, distinct_entries.user_id, GROUP_CONCAT(distinct_entries.hint_id ORDER BY distinct_entries.hint_id ASC) AS processed_hints, distinct_entries.date_accepted, COUNT(distinct_entries.hint_id) AS n
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.f_name, highest_classification_in_window.l_name, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, distinct_attributes.date_type_id, distinct_attributes.date_accepted
#     FROM
#     (SELECT DISTINCT min_date, max_date, bt_hint_date.hint_id, gta_user_log.f_name, gta_user_log.l_name, gta_user_log.user_id, MAX(bt_hint_date.classification_id) AS classification_id, bt_hint_date.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN bt_hint_date 
#       JOIN bt_classification_log ON bt_hint_date.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE date_accepted IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, bt_hint_date.hint_id) highest_classification_in_window
#     JOIN (SELECT bt_hint_date.hint_id, bt_hint_date.classification_id, date, date_type_id, date_accepted FROM bt_hint_date JOIN bt_classification_log ON bt_classification_log.classification_id = bt_hint_date.classification_id AND bt_hint_date.date_accepted IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id, distinct_entries.date_accepted) result_matrix
# JOIN 
# (SELECT distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id, GROUP_CONCAT(distinct_entries.hint_id ORDER BY distinct_entries.hint_id ASC) AS processed_hints, distinct_entries.date, distinct_entries.date_accepted, COUNT(distinct_entries.hint_id) AS N
#   FROM
#   (SELECT DISTINCT highest_classification_in_window.min_date, highest_classification_in_window.max_date, highest_classification_in_window.hint_id, highest_classification_in_window.user_id, highest_classification_in_window.classification_id, highest_classification_in_window.validation_classification, distinct_attributes.date, distinct_attributes.date_type_id, distinct_attributes.date_accepted
#     FROM
#     (SELECT DISTINCT min_date, max_date, bt_hint_date.hint_id, gta_user_log.user_id, MAX(bt_hint_date.classification_id) AS classification_id, bt_hint_date.validation_classification
#       FROM `__increment_days` 
#       CROSS JOIN bt_hint_date 
#       JOIN bt_classification_log ON bt_hint_date.classification_id = bt_classification_log.classification_id AND bt_classification_log.time_stamp >= min_date AND bt_classification_log.time_stamp < max_date
#       JOIN gta_user_log ON bt_classification_log.user_id = gta_user_log.user_id JOIN gta_user_group ON gta_user_log.user_id = gta_user_group.user_id AND gta_user_group.group_id = 3
#       WHERE date_accepted IS NOT NULL
#       GROUP BY min_date, max_date, gta_user_log.user_id, bt_hint_date.hint_id) highest_classification_in_window
#     JOIN (SELECT bt_hint_date.hint_id, bt_hint_date.classification_id, date, date_type_id, date_accepted FROM bt_hint_date JOIN bt_classification_log ON bt_classification_log.classification_id = bt_hint_date.classification_id AND bt_hint_date.date_accepted IS NOT NULL) distinct_attributes 
#     ON distinct_attributes.hint_id = highest_classification_in_window.hint_id AND distinct_attributes.classification_id = highest_classification_in_window.classification_id) distinct_entries
#   GROUP BY distinct_entries.min_date, distinct_entries.max_date, distinct_entries.user_id) N_matrix 
# ON result_matrix.min_date = N_matrix.min_date AND result_matrix.max_date = N_matrix.max_date AND result_matrix.user_id = N_matrix.user_id;
# END IF;
# DROP TABLE `__increment_days`; 
# END$$
#   DELIMITER ;


