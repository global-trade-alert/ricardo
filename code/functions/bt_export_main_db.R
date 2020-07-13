bt_export_main_db=function(to.db = 'ricardodev'){
  
  library(pool)
  library(gtasql)
  library(gtalibrary)
  gta_setwd()
  
  ####### Definitions
  #### Intervention types definition
  intervention.groups=rbind(data.frame(intervention.group = "uncertain",
                                       intervention.type = "Instrument unclear",
                                       stringsAsFactors = F),
                            data.frame(intervention.group = "other",
                                       intervention.type = c("Control on personal transactions", 
                                                             "Controls on commercial transactions and investment instruments", 
                                                             "Controls on credit operations", 
                                                             "Repatriation & surrender requirements", 
                                                             "FDI: Entry and ownership rule", 
                                                             "FDI: Financial incentive", 
                                                             "FDI: Treatment and operations, nes", 
                                                             "Competitive devaluation", 
                                                             "Local labour", 
                                                             "Local operations", 
                                                             "Local sourcing", 
                                                             "Localisation incentive", 
                                                             "Public procurement access", 
                                                             "Public procurement preference margin", 
                                                             "Public procurement localisation", 
                                                             "Public procurement, nes", 
                                                             "Labour market access", 
                                                             "Post-migration treatment", 
                                                             "Intellectual property protection")),
                            data.frame(intervention.group = "export subsidy",
                                       intervention.type = c("Trade finance",
                                                             "Tax-based export incentive",
                                                             "Financial assistance in foreign market",
                                                             "Other export incentive",
                                                             "Export subsidy")),
                            data.frame(intervention.group = "domestic subsidy (incl. tax cuts, rescues etc.)",
                                       intervention.type = c("Capital injection and equity stakes (including bailouts)", 
                                                             "Consumption subsidy", 
                                                             "Financial grant", 
                                                             "In-kind grant", 
                                                             "Interest payment subsidy", 
                                                             "Loan guarantee", 
                                                             "Price stabilisation", 
                                                             "Production subsidy", 
                                                             "State aid, nes", 
                                                             "State loan", 
                                                             "Tax or social insurance relief")),
                            data.frame(intervention.group = "import barrier",
                                       intervention.type = c("Sanitary and phytosanitary measure", 
                                                             "Technical barrier to trade", 
                                                             "Import monitoring", 
                                                             "Anti-circumvention", 
                                                             "Anti-dumping", 
                                                             "Anti-subsidy", 
                                                             "Safeguard", 
                                                             "Special safeguard", 
                                                             "Import licensing requirement", 
                                                             "Import quota", 
                                                             "Import ban", 
                                                             "Import tariff quota", 
                                                             "Internal taxation of imports", 
                                                             "Import incentive", 
                                                             "Import tariff", 
                                                             "Import-related non-tariff measure, nes")),
                            data.frame(intervention.group = "export barrier",
                                       intervention.type = c("Trade payment measure", 
                                                             "Trade balancing measure", 
                                                             "Export ban", 
                                                             "Export quota", 
                                                             "Export tariff quota", 
                                                             "Foreign customer limit", 
                                                             "Export licensing requirement", 
                                                             "Export tax", 
                                                             "Export-related non-tariff measure, nes")))
  
  ## HS code definitions
  hs.all=gtalibrary::hs.codes$hs.code
  hs.food=hs.codes$hs.code[hs.codes$is.covid.food==T]
  hs.equip=hs.codes$hs.code[hs.codes$is.covid.medical.equipment==T]
  hs.consum=c(hs.codes$hs.code[hs.codes$is.covid.medical.supplies==T],hs.codes$hs.code[hs.codes$is.covid.antiepidemic.goods==T])
  hs.drug=hs.codes$hs.code[hs.codes$is.covid.medicines==T]
  hs.other=hs.all[! hs.all %in% c(hs.food,hs.equip,hs.consum,hs.drug)]
  
  hs.groups=rbind(data.frame(hs.group = "food",
                             hs.code = hs.food),
                  data.frame(hs.group = "medical equipment",
                             hs.code = hs.equip),
                  data.frame(hs.group = "medical consumables",
                             hs.code = hs.consum),
                  data.frame(hs.group = "medicines or drugs",
                             hs.code = hs.drug),
                  data.frame(hs.group = "other",
                             hs.code = hs.other))
 
  gta_sql_pool_open(pool.name = "main", db.title="gtamain",
                    db.host = gta_pwd("gtamain")$host,
                    db.name = gta_pwd("gtamain")$name,
                    db.user = gta_pwd("gtamain")$user,
                    db.password = gta_pwd("gtamain")$password,
                    table.prefix = "bt_")
  
  ###### pull data from main
  gta.data=gta_sql_get_value("SELECT gm.id AS act_id, gm.status_id, gi.id AS intervention_id, IF(gi.evaluation_id=2,1,gi.evaluation_id) AS evaluation_id, gj.name AS jurisdiction_name, gm.title, gmt.name AS intervention_type, gm.announcement_date, gi.inception_date, gi.removal_date,  gm.source, gm.is_source_official, gi.description
                      FROM gta_measure gm
                      LEFT JOIN gta_measure_framework gmf ON gmf.measure_id = gm.id
                      JOIN gta_intervention gi ON gi.measure_id = gm.id
                      JOIN gta_measure_type gmt ON gi.measure_type_id = gmt.id
                      JOIN gta_implementing_jurisdiction gij ON gi.id = gij.intervention_id
                      JOIN gta_jurisdiction gj ON gij.jurisdiction_id = gj.id
                      WHERE (gmf.framework_id IN (132,136) OR gm.announcement_date >= '2019-01-01') AND gm.status_id!=5;", "main")
  
  gta.sa.hs=gta_sql_get_value("SELECT gm.id AS act_id, gi.id AS intervention_id, atl.tariff_line_code
                      FROM gta_measure gm
                      LEFT JOIN gta_measure_framework gmf ON gmf.measure_id = gm.id
                      JOIN gta_intervention gi ON gi.measure_id = gm.id
                      JOIN gta_affected_tariff_line atl ON gi.id = atl.intervention_id
                      WHERE (gmf.framework_id IN (132,136) OR gm.announcement_date >= '2019-01-01') AND gm.status_id!=5;","main")
  
  gta_sql_pool_close('main')
  names(gta.sa.hs)=c("state.act.id", "intervention.id","hs6")
  gta.sa.hs$state.act.id=as.numeric(gta.sa.hs$state.act.id)
  gta.sa.hs$hs6=as.numeric(gta.sa.hs$hs6)
  
  
  base.export <<- merge(gta.data, intervention.groups, by = 'intervention.type', all.x = T)
  product.export <<- na.omit(unique(merge(gta.sa.hs, hs.groups, by.x = 'hs6', by.y = 'hs.code', all.x = T)[,c('state.act.id','intervention.id','hs.group')]))
  
  
  # I want to add a pool name with the var to.db but the sql_create_table fails if i do so, so instead i just close the main pool
  if(to.db == 'ricardodev') db.name = 'ricardomainclone' else db.name = gta_pwd(to.db)$name
  gta_sql_pool_open(db.title=to.db,
                    db.host = gta_pwd(to.db)$host,
                    db.name = db.name,
                    db.user = gta_pwd(to.db)$user,
                    db.password = gta_pwd(to.db)$password,
                    table.prefix = "bt_")
  
  gta_sql_get_value("DROP TABLE IF EXISTS bt_product_export;")
  gta_sql_get_value("DROP TABLE IF EXISTS bt_base_export;")
  
  gta_sql_create_table(write.df = 'base.export', append.existing = F)
  gta_sql_create_table(write.df = 'product.export', append.existing = F)
  
  populate.sql.query = paste0("/* SCRIPT TO UPDATE RICARDO DB AND LOCATE CONFLICTS */
                              ALTER TABLE bt_base_export ADD hint_id INT;
                              
                              INSERT INTO bt_hint_log (gta_id, registration_date, acting_agency, hint_date, hint_values, user_id, hint_type_id, hint_state_id, upload_id)
                              SELECT DISTINCT(bt_base_export.intervention_id), DATE(NOW()) registration_date, 'GTA WEBSITE' AS acting_agency, NULL AS hint_date, NULL AS hint_values, 1 AS user_id, 2 AS hint_type_id, 7 AS hint_state_id, NULL AS upload_id
                              FROM bt_base_export 
                              WHERE NOT EXISTS (SELECT NULL FROM bt_hint_log WHERE bt_base_export.intervention_id = bt_hint_log.gta_id);
                              
                              UPDATE bt_base_export
                              JOIN bt_hint_log ON bt_base_export.intervention_id = bt_hint_log.gta_id
                              SET bt_base_export.hint_id = bt_hint_log.hint_id;
                              
                              INSERT INTO bt_hint_state_act(hint_id, state_act_id)
                              SELECT DISTINCT bt_base_export.hint_id, bt_base_export.act_id AS state_act_id FROM bt_base_export
                              WHERE NOT EXISTS (SELECT NULL FROM bt_hint_state_act WHERE bt_hint_state_act.hint_id = bt_base_export.hint_id AND bt_hint_state_act.state_act_id = bt_base_export.act_id);
                              
                              SET @classification_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_classification_log');
                                                        
                              INSERT INTO bt_classification_log(classification_id, user_id, hint_state_id, time_stamp)
                              SELECT DISTINCT @classification_id AS classification_id, 1 AS user_id, (SELECT hint_state_id FROM bt_hint_state_list WHERE bt_hint_state_list.hint_state_name = 'B221 - freelancer desk') AS hint_state_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS time_stamp; 
                              
                              SET @conflict_id = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name='bt_conflict_log');
                                                        
                              INSERT INTO bt_conflict_log(conflict_id, conflict_creation)
                              SELECT @conflict_id AS conflict_id, CONVERT_TZ(NOW(), 'UTC' , 'CET') AS conflict_creation; 
                              
                              /* FIRST TEXT - ONLY DESCRIPTIONS CURRENTLY HANDLED, SHOULD I ALSO DO TITLES? */
                              INSERT INTO bt_hint_text(hint_id, hint_title, hint_description, language_id, classification_id, description_accepted, validation_user)
                              SELECT DISTINCT bt_base_export.hint_id, bt_base_export.title, bt_base_export.description, 1 AS language_id, @classification_id AS classification_id, 1 AS description_accepted, 1 AS validation_user 
                              FROM bt_base_export
                              WHERE NOT EXISTS (SELECT NULL FROM bt_hint_text WHERE bt_base_export.hint_id = bt_hint_text.hint_id AND bt_hint_text.language_id = 1 AND bt_hint_text.description_accepted = 1);
                              
                              # second not exists statement does not re-create a conflict if it already exists regardless of conflict status - unresolved or resolved (should it only avoid unresolved ones?)
                              # same question will exist for all other insert into conflict queries
                              INSERT INTO bt_conflict_text(conflict_id, hint_id, conflict_title, conflict_description, conflict_status, resolution_user)
                              SELECT DISTINCT @conflict_id AS conflict_id, bt_base_export.hint_id, bt_base_export.title AS conflict_title, bt_base_export.description AS conflict_description, 1 AS conflict_status, NULL AS resolution_user
                              FROM bt_base_export
                              WHERE NOT EXISTS (SELECT NULL FROM bt_hint_text WHERE bt_base_export.hint_id = bt_hint_text.hint_id AND bt_hint_text.language_id = 1 AND bt_hint_text.description_accepted = 1 AND bt_base_export.description = bt_hint_text.hint_description)
                              AND NOT EXISTS (SELECT NULL FROM bt_conflict_text WHERE bt_base_export.hint_id = bt_conflict_text.hint_id AND bt_base_export.description = bt_conflict_text.conflict_description);
                              
                              /* RELEVANCE */
                              INSERT INTO bt_hint_relevance(hint_id, classification_id, relevance, relevance_probability, relevance_accepted, validation_user)
                              SELECT DISTINCT bt_base_export.hint_id, @classification_id AS classification_id, IF(bt_base_export.status_id = 5, 0, 1) AS relevance, NULL AS relevance_probability, 1 AS relevance_accepted, 1 AS validation_user 
                              FROM bt_base_export
                              WHERE NOT EXISTS (SELECT NULL FROM bt_hint_relevance WHERE bt_base_export.hint_id = bt_hint_relevance.hint_id AND bt_hint_relevance.relevance_accepted = 1);
                              
                              INSERT INTO bt_conflict_relevance(conflict_id, hint_id, relevance, conflict_status, resolution_user)
                              SELECT DISTINCT @conflict_id AS conflict_id, bt_base_export.hint_id, IF(bt_base_export.status_id = 5, 0, 1) AS relevance, 1 AS conflict_status, NULL AS resolution_user
                              FROM bt_base_export
                              WHERE NOT EXISTS (SELECT NULL FROM bt_hint_relevance WHERE bt_base_export.hint_id = bt_hint_relevance.hint_id AND bt_hint_relevance.relevance_accepted = 1 AND IF(bt_base_export.status_id = 5, 0, 1) = bt_hint_relevance.relevance)
                              AND NOT EXISTS (SELECT NULL FROM bt_conflict_relevance WHERE bt_base_export.hint_id = bt_conflict_relevance.hint_id AND IF(bt_base_export.status_id = 5, 0, 1) = bt_conflict_relevance.relevance);
                              
                              /* ASSESSMENT */
                              INSERT INTO b221_hint_assessment(hint_id, classification_id, assessment_id, assessment_accepted, validation_user)
                              SELECT DISTINCT bt_base_export.hint_id, @classification_id AS classification_id, b221_assessment_list.assessment_id, 1 AS assessment_accepted, 1 AS validation_user 
                              FROM bt_base_export
                              JOIN b221_assessment_list ON bt_base_export.evaluation_id = b221_assessment_list.gta_assessment_id
                              WHERE NOT EXISTS (SELECT NULL FROM b221_hint_assessment WHERE bt_base_export.hint_id = b221_hint_assessment.hint_id AND b221_hint_assessment.assessment_accepted = 1);
                              
                              INSERT INTO bt_conflict_assessment(conflict_id, hint_id, conflict_assessment_id, conflict_status, resolution_user)
                              SELECT DISTINCT @conflict_id AS conflict_id, bt_base_export.hint_id, b221_assessment_list.assessment_id AS conflict_assessment_id, 1 AS conflict_status, NULL AS resolution_user
                              FROM bt_base_export
                              JOIN b221_assessment_list ON bt_base_export.evaluation_id = b221_assessment_list.gta_assessment_id
                              WHERE NOT EXISTS (SELECT NULL FROM b221_hint_assessment WHERE bt_base_export.hint_id = b221_hint_assessment.hint_id AND b221_hint_assessment.assessment_accepted = 1 AND b221_assessment_list.assessment_id = b221_hint_assessment.assessment_id)
                              AND NOT EXISTS (SELECT NULL FROM bt_conflict_assessment WHERE bt_base_export.hint_id = bt_conflict_assessment.hint_id AND b221_assessment_list.assessment_id = bt_conflict_assessment.conflict_assessment_id);
                              
                              /* JURISDICTION */
                              INSERT INTO bt_hint_jurisdiction(hint_id, classification_id, jurisdiction_id, jurisdiction_accepted, validation_user)
                              SELECT DISTINCT bt_base_export.hint_id, @classification_id AS classification_id, gta_jurisdiction_list.jurisdiction_id, 1 AS jurisdiction_accepted, 1 AS validation_user 
                              FROM bt_base_export
                              JOIN gta_jurisdiction_list ON bt_base_export.jurisdiction_name = gta_jurisdiction_list.jurisdiction_name
                              WHERE NOT EXISTS (SELECT NULL FROM bt_hint_jurisdiction WHERE bt_base_export.hint_id = bt_hint_jurisdiction.hint_id AND bt_hint_jurisdiction.jurisdiction_accepted = 1);
                              
                              INSERT INTO bt_conflict_jurisdiction(conflict_id, hint_id, conflict_jurisdiction_id, conflict_status, resolution_user)
                              SELECT DISTINCT @conflict_id AS conflict_id, bt_base_export.hint_id, gta_jurisdiction_list.jurisdiction_id AS conflict_jurisdiction_id, 1 AS conflict_status, NULL AS resolution_user
                              FROM bt_base_export 
                              JOIN (SELECT bt_base_export.hint_id, GROUP_CONCAT(DISTINCT(gta_jurisdiction_list.jurisdiction_id) ORDER BY gta_jurisdiction_list.jurisdiction_id ASC) AS new_values
                              	FROM bt_base_export
                              	JOIN gta_jurisdiction_list ON gta_jurisdiction_list.jurisdiction_name = bt_base_export.jurisdiction_name
                              	GROUP BY bt_base_export.hint_id) new_jur ON bt_base_export.hint_id = new_jur.hint_id
                              JOIN gta_jurisdiction_list ON bt_base_export.jurisdiction_name = gta_jurisdiction_list.jurisdiction_name
                              WHERE NOT EXISTS (SELECT NULL FROM (SELECT bt_hint_jurisdiction.hint_id, GROUP_CONCAT(DISTINCT(bt_hint_jurisdiction.jurisdiction_id) ORDER BY bt_hint_jurisdiction.jurisdiction_id ASC) AS existing_values FROM bt_hint_jurisdiction WHERE bt_hint_jurisdiction.jurisdiction_accepted = 1 GROUP BY bt_hint_jurisdiction.hint_id) existing_jur WHERE new_jur.hint_id = existing_jur.hint_id AND new_jur.new_values = existing_jur.existing_values)
                              AND NOT EXISTS (SELECT NULL FROM (SELECT bt_conflict_jurisdiction.hint_id, GROUP_CONCAT(DISTINCT(bt_conflict_jurisdiction.conflict_jurisdiction_id) ORDER BY bt_conflict_jurisdiction.conflict_jurisdiction_id ASC) AS existing_values FROM bt_conflict_jurisdiction GROUP BY bt_conflict_jurisdiction.conflict_id, bt_conflict_jurisdiction.hint_id) existing_jur WHERE new_jur.hint_id = existing_jur.hint_id AND new_jur.new_values = existing_jur.existing_values);
                              
                              /* PRODUCT GROUP */
                              INSERT INTO b221_hint_product_group(hint_id, classification_id, product_group_id, product_group_assessment, validation_user)
                              SELECT DISTINCT bt_base_export.hint_id, @classification_id AS classification_id, b221_product_group_list.product_group_id AS product_group_id, 1 AS product_group_assessment, 1 AS validation_user 
                              FROM bt_product_export
                              JOIN bt_base_export ON bt_base_export.intervention_id = bt_product_export.intervention_id 
                              JOIN b221_product_group_list ON bt_product_export.hs_group = b221_product_group_list.product_group_name
                              WHERE NOT EXISTS (SELECT NULL FROM b221_hint_product_group WHERE bt_base_export.hint_id = b221_hint_product_group.hint_id AND b221_hint_product_group.product_group_assessment = 1);
                              
                              INSERT INTO bt_conflict_product_group(conflict_id, hint_id, conflict_product_group_id, conflict_status, resolution_user)
                              SELECT DISTINCT @conflict_id AS conflict_id, new_prod.hint_id, b221_product_group_list.product_group_id AS conflict_product_group_id, 1 AS conflict_status, NULL AS resolution_user
                              FROM bt_product_export 
                              JOIN (SELECT bt_base_export.hint_id, bt_product_export.intervention_id, GROUP_CONCAT(DISTINCT(b221_product_group_list.product_group_id) ORDER BY b221_product_group_list.product_group_id ASC) AS new_values
                              	FROM bt_product_export
                              	JOIN bt_base_export ON bt_base_export.intervention_id = bt_product_export.intervention_id
                              	JOIN b221_product_group_list ON bt_product_export.hs_group = b221_product_group_list.product_group_name
                              	GROUP BY bt_base_export.hint_id) new_prod ON bt_product_export.intervention_id = new_prod.intervention_id
                              JOIN b221_product_group_list ON bt_product_export.hs_group = b221_product_group_list.product_group_name
                              WHERE NOT EXISTS (SELECT NULL FROM (SELECT b221_hint_product_group.hint_id, GROUP_CONCAT(DISTINCT(b221_hint_product_group.product_group_id) ORDER BY b221_hint_product_group.product_group_id ASC) AS existing_values FROM b221_hint_product_group WHERE b221_hint_product_group.product_group_assessment = 1 GROUP BY b221_hint_product_group.hint_id) existing_prod WHERE new_prod.hint_id = existing_prod.hint_id AND new_prod.new_values = existing_prod.existing_values)
                              AND NOT EXISTS (SELECT NULL FROM (SELECT bt_conflict_product_group.hint_id, GROUP_CONCAT(DISTINCT(bt_conflict_product_group.conflict_product_group_id) ORDER BY bt_conflict_product_group.conflict_product_group_id ASC) AS existing_values FROM bt_conflict_product_group GROUP BY bt_conflict_product_group.conflict_id, bt_conflict_product_group.hint_id) existing_prod WHERE new_prod.hint_id = existing_prod.hint_id AND new_prod.new_values = existing_prod.existing_values);
                              
                              /* INTERVENTION TYPE */
                              INSERT INTO b221_hint_intervention(hint_id, classification_id, apparent_intervention_id, intervention_accepted, validation_user)
                              SELECT DISTINCT bt_base_export.hint_id, @classification_id AS classification_id, b221_intervention_type_list.intervention_type_id, 1 AS intervention_accepted, 1 AS validation_user 
                              FROM bt_base_export
                              JOIN b221_intervention_type_list ON bt_base_export.intervention_group = b221_intervention_type_list.intervention_type_name
                              WHERE NOT EXISTS (SELECT NULL FROM b221_hint_intervention WHERE bt_base_export.hint_id = b221_hint_intervention.hint_id AND b221_hint_intervention.intervention_accepted = 1);
                              
                              INSERT INTO bt_conflict_intervention(conflict_id, hint_id, conflict_intervention_id, conflict_status, resolution_user)
                              SELECT DISTINCT @conflict_id AS conflict_id, bt_base_export.hint_id, b221_intervention_type_list.intervention_type_id AS conflict_intervention_id, 1 AS conflict_status, NULL AS resolution_user
                              FROM bt_base_export 
                              JOIN (SELECT bt_base_export.hint_id, GROUP_CONCAT(DISTINCT(b221_intervention_type_list.intervention_type_id) ORDER BY b221_intervention_type_list.intervention_type_id ASC) AS new_values
                              	FROM bt_base_export
                              	JOIN b221_intervention_type_list ON b221_intervention_type_list.intervention_type_name = bt_base_export.intervention_group
                              	GROUP BY bt_base_export.hint_id) new_int ON bt_base_export.hint_id = new_int.hint_id
                              JOIN b221_intervention_type_list ON b221_intervention_type_list.intervention_type_name = bt_base_export.intervention_group
                              WHERE NOT EXISTS (SELECT NULL FROM (SELECT b221_hint_intervention.hint_id, GROUP_CONCAT(DISTINCT(b221_hint_intervention.apparent_intervention_id) ORDER BY b221_hint_intervention.apparent_intervention_id ASC) AS existing_values FROM b221_hint_intervention WHERE b221_hint_intervention.intervention_accepted = 1 GROUP BY b221_hint_intervention.hint_id) existing_int WHERE new_int.hint_id = existing_int.hint_id AND new_int.new_values = existing_int.existing_values)
                              AND NOT EXISTS (SELECT NULL FROM (SELECT bt_conflict_intervention.hint_id, GROUP_CONCAT(DISTINCT(bt_conflict_intervention.conflict_intervention_id) ORDER BY bt_conflict_intervention.conflict_intervention_id ASC) AS existing_values FROM bt_conflict_intervention GROUP BY bt_conflict_intervention.conflict_id, bt_conflict_intervention.hint_id) existing_int WHERE new_int.hint_id = existing_int.hint_id AND new_int.new_values = existing_int.existing_values);
                              
                              CREATE INDEX idx_url ON bt_base_export(source(50));
                              
                              /* URL */
                              INSERT INTO bt_url_log(url)
                              SELECT DISTINCT srces.source AS url FROM bt_base_export srces
                              WHERE srces.source IS NOT NULL
                              AND NOT EXISTS
                              (SELECT NULL FROM bt_url_log WHERE bt_url_log.url = srces.source);
                              
                              INSERT INTO bt_hint_url(hint_id, url_id, url_type_id, classification_id, url_accepted, validation_user)
                              SELECT base_urls.hint_id, bt_url_log.url_id, base_urls.url_type_id, @classification_id AS classification_id, 1 AS url_accepted, 1 AS validation_user FROM 
                              (SELECT DISTINCT bt_base_export.hint_id, bt_base_export.source AS url, (CASE WHEN bt_base_export.is_source_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id
                              FROM bt_base_export) base_urls
                              JOIN bt_url_log ON base_urls.url = bt_url_log.url
                              WHERE NOT EXISTS (SELECT NULL FROM bt_hint_url WHERE bt_hint_url.hint_id = base_urls.hint_id AND bt_hint_url.url_accepted = 1);
                               
                              INSERT INTO bt_conflict_url(conflict_id, hint_id, conflict_url_id, conflict_url_type_id, conflict_status, resolution_user)
                              SELECT DISTINCT @conflict_id AS conflict_id, base_urls.hint_id, bt_url_log.url_id, base_urls.url_type_id, 1 AS conflict_status, NULL AS resolution_user
                              FROM (SELECT bt_base_export.hint_id, bt_base_export.source, (CASE WHEN bt_base_export.is_source_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id FROM bt_base_export) base_urls
                              JOIN (SELECT new_base_urls.hint_id, GROUP_CONCAT(bt_url_log.url_id ORDER BY bt_url_log.url_id ASC) AS new_values, GROUP_CONCAT(new_base_urls.url_type_id ORDER BY bt_url_log.url_id ASC) AS new_types
                              	FROM (SELECT bt_base_export.hint_id, bt_base_export.source, (CASE WHEN bt_base_export.is_source_official = 1 THEN (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'official') ELSE (SELECT url_type_id FROM bt_url_type_list WHERE url_type_name = 'news') END) AS url_type_id FROM bt_base_export) new_base_urls
                              	JOIN bt_url_log ON bt_url_log.url = new_base_urls.source
                              	GROUP BY new_base_urls.hint_id) new_src ON base_urls.hint_id = new_src.hint_id
                              JOIN bt_url_log ON bt_url_log.url = base_urls.source
                              WHERE NOT EXISTS (SELECT NULL FROM (SELECT bt_hint_url.hint_id, GROUP_CONCAT(bt_hint_url.url_id ORDER BY bt_hint_url.url_id ASC) AS existing_urls, GROUP_CONCAT(bt_hint_url.url_type_id ORDER BY bt_hint_url.url_id ASC) AS existing_types FROM bt_hint_url WHERE bt_hint_url.url_accepted = 1 GROUP BY bt_hint_url.hint_id) existing_urls WHERE new_src.hint_id = existing_urls.hint_id AND new_src.new_values = existing_urls.existing_urls AND new_src.new_types = existing_urls.existing_types)
                              AND NOT EXISTS (SELECT NULL FROM (SELECT bt_conflict_url.hint_id, GROUP_CONCAT(bt_conflict_url.conflict_url_id ORDER BY bt_conflict_url.conflict_url_id ASC) AS existing_urls, GROUP_CONCAT(bt_conflict_url.conflict_url_type_id ORDER BY bt_conflict_url.conflict_url_id ASC) AS existing_types FROM bt_conflict_url GROUP BY bt_conflict_url.conflict_id, bt_conflict_url.hint_id) existing_url WHERE new_src.hint_id = existing_url.hint_id AND new_src.new_values = existing_url.existing_urls AND new_src.new_types = existing_url.existing_types);
                              
                              /* DATE */
                              INSERT INTO bt_hint_date(hint_id, `date`, date_type_id, classification_id, date_accepted, validation_user)
                              SELECT DISTINCT new_dates.hint_id, `date`, date_type_id, @classification_id AS classification_id, 1 AS date_accepted, 1 AS validation_user 
                              FROM (SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'implementation') AS date_type_id, changes.inception_date AS `date`, 1 as date_accepted, 1 as validation_user
                              FROM bt_base_export changes
                              UNION 
                              SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'announcement') AS date_type_id, changes.announcement_date AS `date`, 1 as date_accepted, 1 as validation_user
                              FROM bt_base_export changes
                              UNION 
                              SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'removal') AS date_type_id, changes.removal_date AS `date`, 1 as date_accepted, 1 as validation_user
                              FROM bt_base_export changes) new_dates
                              WHERE NOT EXISTS (SELECT NULL FROM bt_hint_date WHERE new_dates.hint_id = bt_hint_date.hint_id AND bt_hint_date.date_accepted = 1) AND `date` IS NOT NULL;
                              
                              INSERT INTO bt_conflict_date(conflict_id, hint_id, conflict_date, conflict_date_type_id, conflict_status, resolution_user)
                              SELECT DISTINCT @conflict_id AS conflict_id, long_base_export.hint_id, long_base_export.`date` AS conflict_date, long_base_export.date_type_id, 1 AS conflict_status, NULL AS resolution_user
                              FROM (SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'implementation') AS date_type_id, changes.inception_date AS `date`, 1 as date_accepted, 1 as validation_user
                              	FROM bt_base_export changes
                              	UNION 
                              	SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'announcement') AS date_type_id, changes.announcement_date AS `date`, 1 as date_accepted, 1 as validation_user
                              	FROM bt_base_export changes
                              	UNION 
                              	SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'removal') AS date_type_id, changes.removal_date AS `date`, 1 as date_accepted, 1 as validation_user
                              	FROM bt_base_export changes) long_base_export 
                              JOIN (SELECT DISTINCT new_dates.hint_id, GROUP_CONCAT(new_dates.`date` ORDER BY new_dates.date_type_id ASC) AS new_values
                              	FROM (SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'implementation') AS date_type_id, IF(changes.inception_date IS NULL, 0, changes.inception_date) AS `date`, 1 as date_accepted, 1 as validation_user
                              	FROM bt_base_export changes
                              	UNION 
                              	SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'announcement') AS date_type_id, IF(changes.announcement_date IS NULL, 0, changes.announcement_date) AS `date`, 1 as date_accepted, 1 as validation_user
                              	FROM bt_base_export changes
                              	UNION 
                              	SELECT changes.hint_id, @classification_id AS classification_id, (SELECT bt_date_type_list.date_type_id FROM bt_date_type_list WHERE bt_date_type_list.date_type_name = 'removal') AS date_type_id, IF(changes.removal_date IS NULL, 0, changes.removal_date) AS `date`, 1 as date_accepted, 1 as validation_user
                              	FROM bt_base_export changes) new_dates
                              	GROUP BY hint_id) new_dates ON long_base_export.hint_id = new_dates.hint_id
                              WHERE NOT EXISTS (SELECT NULL FROM (SELECT cartesian_existing_dates.hint_id, GROUP_CONCAT((CASE WHEN bt_hint_date.date_type_id IS NULL THEN 0 ELSE bt_hint_date.`date` END) ORDER BY cartesian_existing_dates.date_type_id) existing_values
                              									FROM (SELECT DISTINCT hint_id, bt_date_type_list.date_type_id FROM bt_base_export, bt_date_type_list) cartesian_existing_dates 
                              									LEFT JOIN bt_hint_date ON bt_hint_date.hint_id = cartesian_existing_dates.hint_id AND bt_hint_date.date_type_id = cartesian_existing_dates.date_type_id
                              									GROUP BY cartesian_existing_dates.hint_id) existing_dates WHERE new_dates.hint_id = existing_dates.hint_id AND new_dates.new_values = existing_dates.existing_values)
                              AND NOT EXISTS (SELECT NULL FROM (SELECT cartesian_existing_dates.conflict_id, cartesian_existing_dates.hint_id, GROUP_CONCAT((CASE WHEN bt_conflict_date.conflict_date_type_id IS NULL THEN 0 ELSE bt_conflict_date.conflict_date END) ORDER BY cartesian_existing_dates.date_type_id) existing_values
                              									FROM (SELECT DISTINCT conflict_id, hint_id, bt_date_type_list.date_type_id FROM bt_conflict_date, bt_date_type_list) cartesian_existing_dates 
                              									LEFT JOIN bt_conflict_date ON cartesian_existing_dates.conflict_id = bt_conflict_date.conflict_id AND bt_conflict_date.hint_id = cartesian_existing_dates.hint_id AND bt_conflict_date.conflict_date_type_id = cartesian_existing_dates.date_type_id
                              									GROUP BY cartesian_existing_dates.conflict_id, cartesian_existing_dates.hint_id) existing_date WHERE new_dates.hint_id = existing_date.hint_id AND new_dates.new_values = existing_date.existing_values)
                              AND long_base_export.`date` IS NOT NULL;")
  
  gta_sql_multiple_queries(populate.sql.query, output.queries = 1, show.time = T)
  
  gta_sql_get_value("DROP TABLE IF EXISTS bt_product_export;")
  gta_sql_get_value("DROP TABLE IF EXISTS bt_base_export;")
  
  gta_sql_pool_close()
  
  
}