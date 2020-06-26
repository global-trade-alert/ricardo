rm(list = ls())

library(gtasql)
library(gtalibrary)
library(pool)
library(RMariaDB)
library(data.table)
library(gtabastiat)
library(xlsx)

gta_sql_kill_connections()

database <<- "ricardomain"

gta_sql_pool_open(db.title="ricardomainclone",
                  db.host = "gta-ricardo-dev.cp7esvs8xwum.eu-west-1.rds.amazonaws.com",
                  db.name = 'ricardomainclone',
                  db.user = 'gtaricardodev',
                  db.password = 'nC6okGiDKEcFV36rKsykeE9HXbfphgAH6',
                  table.prefix = "bt_")


project.path="0 projects/49 COVID tracking in B221"


##### get RIC data
ricardo.data=gta_sql_get_value("SELECT bhl.hint_id, bhl.hint_state_id, bhb.bid, gjl.jurisdiction_name, b2al.assessment_name, bht.hint_title, bht.hint_description, butl.url_type_name, bul.url, bhl.acting_agency
                               FROM bt_hint_log bhl
                               JOIN bt_hint_bid bhb
                               ON bhl.hint_id = bhb.hint_id
                               JOIN bt_hint_jurisdiction bhj
                               ON bhl.hint_id = bhj.hint_id
                               JOIN gta_jurisdiction_list gjl
                               ON bhj.jurisdiction_id = gjl.jurisdiction_id
                               JOIN b221_hint_assessment b2ha
                               ON bhl.hint_id = b2ha.hint_id
                               JOIN b221_assessment_list b2al
                               ON b2ha.assessment_id=b2al.assessment_id
                               JOIN bt_hint_text bht
                               ON bhl.hint_id = bht.hint_id 
                               JOIN bt_hint_url bhu
                               ON bhl.hint_id = bhu.hint_id
                               JOIN bt_url_log bul
                               ON bhu.url_id = bul.url_id
                               JOIN bt_url_type_list butl
                               ON bhu.url_type_id = butl.url_type_id
                               WHERE bhl.hint_state_id IN (3,4,5,6,7)
                               AND bhl.hint_type_id=2
                               AND bhj.jurisdiction_accepted = 1
                               AND b2ha.assessment_accepted = 1
                               AND bhu.url_accepted=1
                               AND bht.language_id=1;")

ricardo.data=unique(ricardo.data)

Encoding(ricardo.data$hint.title)="UTF-8"
Encoding(ricardo.data$hint.description)="UTF-8"


# no assessment check 
# no.assessment.entry=no.assessment[,c("hint.id","bid")]
# no.assessment.entry$entry.id=as.numeric(as.character(gsub("\\D+","",no.assessment.entry$bid )))


hint.instrument=gta_sql_get_value(paste0("SELECT bhl.hint_id, b2itl.intervention_type_name
                                          FROM bt_hint_log bhl
                                          JOIN b221_hint_intervention b2hi
                                          ON bhl.hint_id = b2hi.hint_id
                                          JOIN b221_intervention_type_list b2itl
                                          ON b2hi.apparent_intervention_id = b2itl.intervention_type_id
                                          WHERE b2hi.intervention_accepted=1
                                          AND bhl.hint_id IN (",paste(unique(ricardo.data$hint.id), collapse=","),");"))

hint.instrument=unique(hint.instrument)





hint.product=gta_sql_get_value(paste0("SELECT bhl.hint_id, b2pgl.product_group_name
                                       FROM bt_hint_log bhl
                                       JOIN b221_hint_product_group b2hpg
                                       ON bhl.hint_id = b2hpg.hint_id
                                       JOIN b221_product_group_list b2pgl
                                       ON b2hpg.product_group_id = b2pgl.product_group_id
                                       WHERE b2hpg.product_group_assessment=1
                                       AND bhl.hint_id IN (",paste(unique(ricardo.data$hint.id), collapse=","),");"))

hint.product=unique(hint.product)



hint.date=gta_sql_get_value(paste0("SELECT bhd.hint_id, bhd.date, bdtl.date_type_name
                                    FROM bt_hint_date bhd
                                    JOIN bt_date_type_list bdtl
                                    ON bhd.date_type_id=bdtl.date_type_id
                                    WHERE bhd.date_accepted=1
                                    AND bhd.hint_id IN (",paste(unique(ricardo.data$hint.id), collapse=","),");"))

hint.date=unique(hint.date)



#### get GTA data

database <<- "gtamain"

gta_sql_pool_open(pool.name = "main" , 
                  db.title=database,
                  db.host = gta_pwd(database)$host,
                  db.name = gta_pwd(database)$name,
                  db.user = gta_pwd(database)$user,
                  db.password = gta_pwd(database)$password,
                  table.prefix = "gta_")


######### retrieving GTA info
gta.data=gta_sql_get_value("SELECT gm.id, gm.status_id, gi.id, gi.evaluation_id, gj.name, gm.title, gmt.name, gm.announcement_date, inception_date, removal_date,  gm.source, gm.is_source_official, gi.description
                      FROM gta_measure gm
                      JOIN gta_measure_framework gmf
                      ON gmf.measure_id = gm.id
                      JOIN gta_intervention gi
                      ON gi.measure_id = gm.id
                      JOIN gta_measure_type gmt
                      ON gi.measure_type_id = gmt.id
                      JOIN gta_implementing_jurisdiction gij
                      ON gi.id = gij.intervention_id
                      JOIN gta_jurisdiction gj
                      ON gij.jurisdiction_id = gj.id
                      WHERE gmf.framework_id IN (132,136)
                      AND gm.status_id!=5;", "main")

names(gta.data)=c("state.act.id","status.id","intervention.id","gta.evaluation", "country", "title", "intervention.type","announcement.date","inception.date", "removal.date","source", "source.is.official","description")
gta.data$gta.evaluation[gta.data$gta.evaluation==1]="Red"
gta.data$gta.evaluation[gta.data$gta.evaluation==2]="Amber"
gta.data$gta.evaluation[gta.data$gta.evaluation==3]="Green"

Encoding(gta.data$source)="UTF-8"
Encoding(gta.data$description)="UTF-8"

## correcting for EU/EEU
gta.supra=gta_sql_get_value("SELECT measure_id, id 
                            FROM gta_intervention 
                            WHERE implementation_level_id=1;","main")
names(gta.supra)=c("state.act.id","intervention.id")

gta.data$country[gta.data$intervention.id %in% gta.supra$intervention.id & gta.data$intervention.id %in% subset(gta.data, country=="Luxembourg")$intervention.id]="European Union"
gta.data$country[gta.data$intervention.id %in% gta.supra$intervention.id & gta.data$intervention.id %in% subset(gta.data, country=="Russia")$intervention.id]="Eurasian Economic Union"
gta.data$country[gta.data$intervention.id %in% gta.supra$intervention.id & gta.data$intervention.id %in% subset(gta.data, country=="Lesotho")$intervention.id]="Southern African Customs Union"
gta.data=unique(gta.data)

unique(gta.data$country[gta.data$intervention.id %in% gta.supra$intervention.id])


## remove td.cases
td.sa=c(31820, 748, 759)

gta.data=subset(gta.data, ! state.act.id %in% td.sa)

## remove specific interventions
int.rm=c(79104, 79259, 58641, 79143, 79486, 79478, 79479,79477)

gta.data=subset(gta.data, ! intervention.id %in% int.rm)

### Product groups
## HS code definitions
hs.all=gtalibrary::hs.codes$hs.code
hs.food=hs.codes$hs.code[hs.codes$is.covid.food==T]
hs.equip=hs.codes$hs.code[hs.codes$is.covid.medical.equipment==T]
hs.consum=c(hs.codes$hs.code[hs.codes$is.covid.medical.supplies==T],hs.codes$hs.code[hs.codes$is.covid.antiepidemic.goods==T])
hs.drug=hs.codes$hs.code[hs.codes$is.covid.medicines==T]
hs.other=hs.all[! hs.all %in% c(hs.food,hs.equip,hs.consum,hs.drug)]

gta.sa.hs=gta_sql_get_value("SELECT gm.id, gi.id, atl.tariff_line_code
                      FROM gta_measure gm
                      JOIN gta_measure_framework gmf
                      ON gmf.measure_id = gm.id
                      JOIN gta_intervention gi
                      ON gi.measure_id = gm.id
                      JOIN gta_affected_tariff_line atl
                      ON gi.id = atl.intervention_id
                      WHERE gmf.framework_id IN (132,136)
                      AND gm.status_id!=5;","main")
names(gta.sa.hs)=c("state.act.id", "intervention.id","hs6")
gta.sa.hs$state.act.id=as.numeric(gta.sa.hs$state.act.id)
gta.sa.hs$hs6=as.numeric(gta.sa.hs$hs6)

sa.food=unique(gta.sa.hs$state.act.id[as.numeric(as.character(gta.sa.hs$hs6)) %in% hs.food])
sa.equip=unique(gta.sa.hs$state.act.id[as.numeric(as.character(gta.sa.hs$hs6)) %in% hs.equip])
sa.consum=unique(gta.sa.hs$state.act.id[as.numeric(as.character(gta.sa.hs$hs6)) %in% hs.consum])
sa.drug=unique(gta.sa.hs$state.act.id[as.numeric(as.character(gta.sa.hs$hs6)) %in% hs.drug])
sa.other=unique(gta.sa.hs$state.act.id[as.numeric(as.character(gta.sa.hs$hs6)) %in% hs.other])

## interventions without HS code
missing.hs=unique(gta.data$intervention.id)[! unique(gta.data$intervention.id) %in% gta.sa.hs$intervention.id]
is.med.con=c(78883,78982,79095,79094, 79191, 79308, 79306, 79329, 79309, 79298,79389,79390)
is.med.eqm=c(78883,78982,79042,79043,79095,79094, 79191, 79308, 79298, 79306, 79299, 79329, 79309, 79084,79389,79390)
is.med.drug=c(78871,78883,78982, 79024,79191, 79308, 79329, 79309, 79367,79476, 79436)
is.food=c(78990, 79255,79373, 79298)
is.other=c(79019, 73754, 79058, 79057, 79059, 79058, 79056, 61769,62514, 12542, 58887, 59221, 79324,79415)


is.irrelevant.for.this=c()
missing.hs=missing.hs[! missing.hs %in% c(is.med.con, is.med.eqm ,is.med.drug, is.food, is.other,is.irrelevant.for.this)]


gta.product=data.frame()
gta.product=rbind(gta.product,
                  data.frame(intervention.id=unique(c(is.med.con,gta.data$intervention.id[gta.data$state.act.id %in% sa.consum])),
                             product.group.name="medical consumables",
                             stringsAsFactors = F))
gta.product=rbind(gta.product,
                  data.frame(intervention.id=unique(c(is.med.eqm,gta.data$intervention.id[gta.data$state.act.id %in% sa.equip])),
                             product.group.name="medical equipment",
                             stringsAsFactors = F))
gta.product=rbind(gta.product,
                  data.frame(intervention.id=unique(c(is.med.drug,gta.data$intervention.id[gta.data$state.act.id %in% sa.drug])),
                             product.group.name="medicines or drugs",
                             stringsAsFactors = F))
gta.product=rbind(gta.product,
                  data.frame(intervention.id=unique(c(is.food,gta.data$intervention.id[gta.data$state.act.id %in% sa.food])),
                             product.group.name="food",
                             stringsAsFactors = F))
gta.product=rbind(gta.product,
                  data.frame(intervention.id=unique(c(is.other,gta.data$intervention.id[gta.data$state.act.id %in% sa.other])),
                             product.group.name="other",
                             stringsAsFactors = F))


#### Intervention types
int.exp.barrier=c("Export licensing requirement","Export ban","Export quota","Export tax","Export-related non-tariff measure, nes" )
int.imp.barrier=c("Import ban", "Import tariff", "Internal taxation of imports","Import licensing requirement","Import-related non-tariff measure, nes","Anti-dumping","Import tariff quota", "Safeguard")
int.dom.subsidy=c( "Tax or social insurance relief","State loan" ,"Financial grant","Loan guarantee" ,"State aid, nes","Interest payment subsidy","Capital injection and equity stakes (including bailouts)","In-kind grant","Production subsidy" , "Price stabilisation"  )
int.exp.subsidy=c("Trade finance","Tax-based export incentive","Financial assistance in foreign market","Other export incentive" ,"Export subsidy"  )
int.other=c("Controls on credit operations", "Repatriation & surrender requirements" , "Public procurement, nes","Controls on commercial transactions and investment instruments","FDI: Entry and ownership rule", "FDI: Financial incentive","Public procurement preference margin","Labour market access","Public procurement localisation","Post-migration treatment","Local sourcing")
int.unclear=c("Instrument unclear")

if(any(! unique(gta.data$intervention.type) %in% c(int.imp.barrier,int.exp.barrier,int.dom.subsidy,int.exp.subsidy, int.other, int.unclear))){
  stop(paste0("Need to classify a GTA intervention type into the proper instrument category: ",
              paste(unique(gta.data$intervention.type)[! unique(gta.data$intervention.type) %in% c(int.imp.barrier,int.exp.barrier,int.dom.subsidy,int.exp.subsidy, int.other)],  collapse="; ")))
}

gta.intervention=data.frame()

gta.intervention=rbind(gta.intervention,
                  data.frame(intervention.id=unique(gta.data$intervention.id[gta.data$intervention.type %in% int.exp.barrier]),
                             intervention.type.name="export barrier",
                             stringsAsFactors = F))
gta.intervention=rbind(gta.intervention,
                       data.frame(intervention.id=unique(gta.data$intervention.id[gta.data$intervention.type %in% int.imp.barrier]),
                                  intervention.type.name="import barrier",
                                  stringsAsFactors = F))
gta.intervention=rbind(gta.intervention,
                       data.frame(intervention.id=unique(gta.data$intervention.id[gta.data$intervention.type %in% int.dom.subsidy]),
                                  intervention.type.name="domestic subsidy (incl. tax cuts, rescues etc.)",
                                  stringsAsFactors = F))
gta.intervention=rbind(gta.intervention,
                       data.frame(intervention.id=unique(gta.data$intervention.id[gta.data$intervention.type %in% int.exp.subsidy]),
                                  intervention.type.name="export subsidy",
                                  stringsAsFactors = F))
gta.intervention=rbind(gta.intervention,
                       data.frame(intervention.id=unique(gta.data$intervention.id[gta.data$intervention.type %in% int.other]),
                                  intervention.type.name="other",
                                  stringsAsFactors = F))
gta.intervention=rbind(gta.intervention,
                       data.frame(intervention.id=unique(gta.data$intervention.id[gta.data$intervention.type %in% int.unclear]),
                                  intervention.type.name="unclear",
                                  stringsAsFactors = F))




#### CREATING XLSX

## fleshing out GTA part of deliverable
covid.data=data.frame(entry.id=gta.data$intervention.id,
                      entry.type="GTA - published",
                      country=gta.data$country,
                      initial.assessment="restrictive",
                      gta.intervention.type=gta.data$intervention.type,
                      date.announced=gta.data$announcement.date,
                      date.implemented=gta.data$inception.date,
                      date.removed=gta.data$removal.date,
                      description=gta.data$description,
                      source=gta.data$source,
                      intervention.type=gta.data$intervention.type,
                      stringsAsFactors = F)

covid.data$entry.type[covid.data$entry.id %in% subset(gta.data, status.id!=4)$intervention.id]="GTA - under analysis"
table(covid.data$entry.type)
covid.data$initial.assessment[covid.data$entry.id %in% subset(gta.data, gta.evaluation=="Green")$intervention.id]="liberalising"
table(covid.data$initial.assessment)

covid.data$inst.export.barrier=covid.data$entry.id %in% gta.intervention$intervention.id[gta.intervention$intervention.type.name=="export barrier"]
covid.data$inst.import.barrier=covid.data$entry.id %in% gta.intervention$intervention.id[gta.intervention$intervention.type.name=="import barrier"]
covid.data$inst.domestic.subsidy=covid.data$entry.id %in% gta.intervention$intervention.id[gta.intervention$intervention.type.name=="domestic subsidy (incl. tax cuts, rescues etc.)"]
covid.data$inst.export.subsidy=covid.data$entry.id %in% gta.intervention$intervention.id[gta.intervention$intervention.type.name=="export subsidy"]
covid.data$inst.other=covid.data$entry.id %in% gta.intervention$intervention.id[gta.intervention$intervention.type.name=="other"]
covid.data$inst.unclear=covid.data$entry.id %in% gta.intervention$intervention.id[gta.intervention$intervention.type.name=="unclear"]

## special case 78990
covid.data$inst.export.barrier[covid.data$entry.id==78990]=F
covid.data$inst.domestic.subsidy[covid.data$entry.id==78990]=T
table(covid.data$inst.export.barrier)
table(covid.data$inst.import.barrier)

covid.data$prd.med.con=covid.data$entry.id %in% gta.product$intervention.id[gta.product$product.group.name=="medical consumables"]
covid.data$prd.med.eqm=covid.data$entry.id %in% gta.product$intervention.id[gta.product$product.group.name=="medical equipment"]
covid.data$prd.med.drug=covid.data$entry.id %in% gta.product$intervention.id[gta.product$product.group.name=="medicines or drugs"]
covid.data$prd.food=covid.data$entry.id %in% gta.product$intervention.id[gta.product$product.group.name=="food"]
covid.data$prd.other=covid.data$entry.id %in% gta.product$intervention.id[gta.product$product.group.name=="other"]

covid.data$prd.med.any=F
covid.data$prd.med.any[covid.data$prd.med.drug==T]=T
covid.data$prd.med.any[covid.data$prd.med.eqm==T]=T
covid.data$prd.med.any[covid.data$prd.med.con==T]=T


## special fixes from SE & PL check 
no.food=c(79369, 79299, 78916, 79125) # 79362, 79355, 79351, 79346, 79332, 79314, 79304, 79294, 79277, 79275, 79264, 79246, 79245, 79243, 79242, 79241, 79240, 79233, 79183, 79182, 79142, 79133, 79099, 79086, 79085, 79070, 79051, 79045, 79032, 79029, 79028, 79025, 78993, 78992, 78991, 78986, 78981, 78979, 78955, 78941, 78932, 78928, 78910, 78897, 78894, 78880, 78876, 78875, 78869, 78868, 78838, 78836, 78821, 78807, 78802, 78798, 78797)
no.consum= c(79363, 79082,78914, 79321, 79319, 79316, 79297, 79296, 79247, 79231, 79230, 79228, 79226, 79222, 79219, 79164, 79052, 79027, 79023, 79012, 78970, 78926, 78911, 78907, 78896, 78889, 78865, 78841, 78824, 78814, 78813)

covid.data$prd.med.con[covid.data$entry.id %in% no.consum]=F
covid.data$prd.med.eqm[covid.data$entry.id %in% c(79342)]=F
covid.data$prd.med.drug[covid.data$entry.id %in% c(79082, 79346)]=F
covid.data$prd.food[covid.data$entry.id %in% no.food]=F
covid.data$prd.med.any[covid.data$entry.id %in% c(79363, 79082,78914)]=F

###  dates
covid.data$date.announced=as.Date(covid.data$date.announced)
covid.data$date.implemented=as.Date(covid.data$date.implemented)
covid.data$date.removed=as.Date(covid.data$date.removed)




#### Adding RIC to the set
## remove those associated to state acts (and their collections)
hint.state.act=unique(c(gta_sql_get_value("SELECT DISTINCT(hint_id) 
                                          FROM bt_hint_state_act;"),
                        gta_sql_get_value("SELECT DISTINCT(hint_id) 
                                          FROM b221_hint_collection
                                          WHERE collection_id IN (SELECT collection_id 
                                                                  FROM b221_hint_collection
                                                                  WHERE hint_id IN (SELECT hint_id FROM bt_hint_state_act));")))

ricardo.data=subset(ricardo.data, ! hint.id %in% hint.state.act)


## correct for collections, 
## (1) identify starred hints (or state acts)
## (2) removing all hints that belong to a collection except starred hints

collections.to.fix=data.frame(collection.id=c(137,146),
                              starred.hint=c(460, 1098))


hint.collection=gta_sql_get_value("SELECT * FROM b221_hint_collection")
collection.star=gta_sql_get_value("SELECT * FROM b221_collection_star")

no.star=subset(hint.collection, ! collection.id %in% collection.star$collection.id)
no.star=subset(no.star, collection.id %in% subset(hint.collection, hint.id %in% subset(hint.instrument, intervention.type.name %in% c("export barrier","import barrier"))$hint.id)$collection.id)
star.collection=merge(no.star, subset(ricardo.data, hint.id %in% no.star$hint.id), by="hint.id", all.x = T)

all.hints.in.col=hint.collection$hint.id
remove.col.dups=all.hints.in.col[! all.hints.in.col %in% c(collection.star$hint.id, collections.to.fix$starred.hint)]


ricardo.data=subset(ricardo.data, ! hint.id %in% remove.col.dups)

hint.sa=gta_sql_get_value("SELECT * FROM bt_hint_state_act;")

hint.bid=gta_sql_get_value("SELECT * FROM bt_hint_bid;")
hint.bid$entry.id=as.numeric(gsub("\\D+","", hint.bid$bid))
hint.bid=subset(hint.bid, grepl("COVID-GS",bid))
ricardo.data=merge(ricardo.data, hint.bid[,c("hint.id","entry.id")], by="hint.id", all.x=T)

# ricardo.app.data=subset(ricardo.data, is.na(entry.id))
# ricardo.app.data$entry.id=ricardo.app.data$hint.id
# ricardo.app.data$date.announced=ricardo.app.data$hint.date
# ricardo.app.data$date.implemented=NA
# ricardo.app.data$date.removed=NA
# 
# ricardo.data=subset(ricardo.data, is.na(entry.id)==F)


### cleaning

covid.gs=read.csv(paste0(project.path, "/data/GS full sheet 200507.csv"), stringsAsFactors = F, sep=";")
names(covid.gs)[1]="entry.id"


## add descriptions from GS
ricardo.data=merge(ricardo.data, covid.gs[,c("entry.id", "act.description.en")], by="entry.id", all.x=T)
ricardo.data$hint.description=ricardo.data$hint.description
ricardo.data$hint.description[! is.na(ricardo.data$act.description.en)]=ricardo.data$act.description.en[! is.na(ricardo.data$act.description.en)]


## dates
hint.date=reshape(hint.date, idvar="hint.id", timevar = "date.type.name", direction = "wide")
names(hint.date)=c("hint.id", "date.announced","date.implemented","date.removed")
hint.date$date.announced=as.Date(hint.date$date.announced, "%Y-%m-%d")
hint.date$date.implemented=as.Date(hint.date$date.implemented, "%Y-%m-%d")
hint.date$date.removed=as.Date(hint.date$date.removed, "%Y-%m-%d")


ricardo.data=merge(ricardo.data, hint.date, by="hint.id", all.x = T)


## add the dates from GS [map entry to HINT!!]
if(length(unique(subset(ricardo.data, is.na(date.announced) & is.na(date.implemented) & is.na(date.removed) & is.na(entry.id))$hint.id))>0){
  unique(subset(ricardo.data, is.na(date.announced) & is.na(date.implemented) & is.na(date.removed) & is.na(entry.id))$hint.id)
  stop("HINT IDs without a single date")
}

ric.no.date=subset(ricardo.data, is.na(date.announced) & is.na(date.implemented) & is.na(date.removed) & ! is.na(entry.id))
ric.no.date$date.announced=NULL
ric.no.date$date.implemented=NULL
ric.no.date$date.removed=NULL


ric.no.date=merge(ric.no.date, covid.gs[,c("entry.id", "date.announced","date.implemented","date.removed")], by="entry.id", all.x=T)
ric.no.date$date.announced=as.Date(ric.no.date$date.announced, "%d.%m.%Y")
ric.no.date$date.implemented=as.Date(ric.no.date$date.implemented, "%d.%m.%Y")
ric.no.date$date.removed=as.Date(ric.no.date$date.removed, "%d.%m.%Y")

if(nrow(subset(ric.no.date, (is.na(date.announced) & is.na(date.implemented) & is.na(date.removed))))>0){
   still.no.date=subset(ric.no.date, (is.na(date.announced) & is.na(date.implemented) & is.na(date.removed)))
   stop("STILL HINT IDs without a single date")
}


ricardo.data=rbind(subset(ricardo.data, ! hint.id %in% ric.no.date$hint.id),
                   subset(ric.no.date, !(is.na(date.announced) & is.na(date.implemented) & is.na(date.removed))))

## text cleaning

## re-import proper sources from GS
## (convert bid's to entry IDs)
# ricardo.data=merge(ricardo.data, covid.gs[,c("entry.id", "source.name","source.document")], by="entry.id", all.x=T)

  
  
## form product/inst columns
# names(ricardo.data)[!names(ricardo.data) %in% names(ricardo.app.data)]
# SOP FOR NOW ricardo.data=rbind(ricardo.data, ricardo.app.data)

## compiling
covid.data.ric=data.frame(entry.id=ricardo.data$entry.id,
                          hint.id=ricardo.data$hint.id,
                          entry.type=ricardo.data$url.type.name,
                          country=ricardo.data$jurisdiction.name,
                          initial.assessment=ricardo.data$assessment.name,
                          gta.intervention.type=NA,
                          date.announced=ricardo.data$date.announced,
                          date.implemented=ricardo.data$date.implemented,
                          date.removed=ricardo.data$date.removed,
                          description=ricardo.data$hint.description,
                          source=ricardo.data$url,
                          intervention.type=NA,
                          stringsAsFactors = F)

covid.data.ric$entry.type[covid.data.ric$entry.type=="official"]="Official source"
covid.data.ric$entry.type[covid.data.ric$entry.type=="news"]="Non-official source"

table(covid.data.ric$entry.type)

covid.data.ric=subset(covid.data.ric, initial.assessment!="unclear")

covid.data.ric$initial.assessment[covid.data.ric$initial.assessment=="harmful"]="restrictive"
table(covid.data.ric$initial.assessment)

covid.data.ric$inst.export.barrier=covid.data.ric$hint.id %in% hint.instrument$hint.id[hint.instrument$intervention.type.name=="export barrier"]
covid.data.ric$inst.import.barrier=covid.data.ric$hint.id %in%  hint.instrument$hint.id[hint.instrument$intervention.type.name=="import barrier"]
covid.data.ric$inst.domestic.subsidy=covid.data.ric$hint.id %in% hint.instrument$hint.id[hint.instrument$intervention.type.name=="domestic subsidy (incl. tax cuts, rescues etc.)"]
covid.data.ric$inst.export.subsidy=covid.data.ric$hint.id %in% hint.instrument$hint.id[hint.instrument$intervention.type.name=="export subsidy"]
covid.data.ric$inst.other=covid.data.ric$hint.id %in% hint.instrument$hint.id[hint.instrument$intervention.type.name=="other"]
covid.data.ric$inst.unclear=F


table(covid.data.ric$inst.export.barrier)
table(covid.data.ric$inst.import.barrier)



covid.data.ric$prd.med.con=covid.data.ric$hint.id %in% hint.product$hint.id[hint.product$product.group.name=="medical consumables"]
covid.data.ric$prd.med.eqm=covid.data.ric$hint.id %in% hint.product$hint.id[hint.product$product.group.name=="medical equipment"]
covid.data.ric$prd.med.drug=covid.data.ric$hint.id %in% hint.product$hint.id[hint.product$product.group.name=="medicines or drugs"]
covid.data.ric$prd.food=covid.data.ric$hint.id %in% hint.product$hint.id[hint.product$product.group.name=="food"]
covid.data.ric$prd.other=covid.data.ric$hint.id %in% hint.product$hint.id[hint.product$product.group.name=="other"]



covid.data.ric$prd.med.any=F
covid.data.ric$prd.med.any[covid.data.ric$prd.med.drug==T]=T
covid.data.ric$prd.med.any[covid.data.ric$prd.med.eqm==T]=T
covid.data.ric$prd.med.any[covid.data.ric$prd.med.con==T]=T


table(covid.data.ric$prd.med.any)
table(covid.data.ric$prd.food)

covid.data$hint.id=covid.data$entry.id

names(covid.data.ric)[!names(covid.data.ric) %in% names(covid.data)]
names(covid.data)[!names(covid.data) %in% names(covid.data.ric)]

### GENERATE XLSX
covid.data=rbind(covid.data, covid.data.ric)

## all entries cleaning
### country names
covid.data$country[covid.data$country=="Taiwan"]="Chinese Taipei"
covid.data$country[covid.data$country=="South Korea"]="Republic of Korea"
covid.data$country[covid.data$country=="United States of America"]="United States"
covid.data$country[covid.data$country=="Botswana"]="Eswatini"
covid.data$country[grepl("Ivoire", covid.data$country)] = "Ivory Coast"

wb.countries=read.csv("0 projects/49 COVID tracking in B221/help files/WB country names.csv", sep=";", stringsAsFactors = F)
names(wb.countries)[1]="gta.name"
Encoding(wb.countries$wb.name)="UTF-8"

covid.data.names=subset(covid.data, country %in% wb.countries$gta.name)
covid.data.names=merge(covid.data.names, wb.countries, by.x="country", by.y="gta.name", all.x=T)
covid.data.names$country=covid.data.names$wb.name
covid.data.names$wb.name=NULL

covid.data=rbind(subset(covid.data, ! country %in% wb.countries$gta.name),
                 covid.data.names)
rm(covid.data.names, wb.countries)


## med product check
covid.data$prd.med.any=F
covid.data$prd.med.any[covid.data$prd.med.drug==T]=T
covid.data$prd.med.any[covid.data$prd.med.eqm==T]=T
covid.data$prd.med.any[covid.data$prd.med.con==T]=T



## encoding
covid.data$description=gsub("<.*?>","",iconv(covid.data$description, "", "ASCII", "byte"))
covid.data$description=textutils::HTMLdecode(covid.data$description) 
covid.data$description=gsub("<.*?>", "", covid.data$description)


covid.data$source=gsub("<.*?>","",iconv(covid.data$source, "", "ASCII", "byte"))
covid.data$source=textutils::HTMLdecode(covid.data$source) 
covid.data$source=gsub("<.*?>", "", covid.data$source)



## 200501 fixes
# covid.data$description[covid.data$entry.id==1439]="On 25 April 2020 the government of Tajikistan banned exports of certain grains (including wheat and buckwheat) and pulses."
covid.data$description=gsub("^([Rr]estrictive: )","",covid.data$description)
covid.data$description=gsub("^([Ll]iberalisation: )","",covid.data$description)
covid.data$description=gsub("^([Ll]iberalising: )","",covid.data$description)


## Manually adjust dates when implementation date before 2020 so it fits the maps
covid.data$date.implemented[covid.data$entry.id==73754] = as.Date("2020-03-31")
covid.data$date.implemented[covid.data$entry.id==73750] = as.Date("2020-03-31")
covid.data$date.implemented[covid.data$entry.id==61414] = as.Date("2020-03-31")
covid.data$date.implemented[covid.data$entry.id==73749] = as.Date("2020-03-31")
covid.data$date.implemented[covid.data$entry.id==73322] = as.Date("2020-03-31")
covid.data$date.implemented[covid.data$entry.id==79193] = as.Date("2020-03-19")
covid.data$date.implemented[covid.data$entry.id==79194] = as.Date("2020-03-19")
covid.data$date.implemented[covid.data$entry.id==79192] = as.Date("2020-03-19")
covid.data$date.implemented[covid.data$entry.id==79189] = as.Date("2020-03-19")

## changed GTA descriptions
covid.data$description[covid.data$entry.id==59221]="On 1 April 2015, the new Foreign Trade Policy 2015-2020 was announced. The policy has withdrawn the exemption from anti-dumping or safeguard duties that were provided to imports of capital goods under the Export Promotion Capital Goods (EPCG) scheme. The amendment has been made to incentivize domestic procurement of capital goods.
The EPCG scheme allows duty-free import of capital goods that will be used to manufacture export goods.
On 31 March 2020, the Ministry of Commerce and Industry announced that the existing Foreign Trade Policy 2015-2020, which was to expire on 31 March 2020, will be extended till 31 March 2021."

covid.data$description[covid.data$entry.id==79372]="On 30 April 2020, the United Kingdom exempted imports from non-EU countries of certain medical supplies, equipment, and protective garments from VAT in response to the COVID-19 outbreak. Additionally, import duties were also eliminated on the subject goods, see related intervention.
The goods that are granted VAT exemption are set out in the COVID-9 Commodity Code list. With this decision, 76 goods are added to the commodity list. Previously on March 31, 2020, the government eliminated the VAT of 90 goods (see related state act).
The goods must be imported by state entities 'including state bodies, public bodies and other bodies governed by public law'. Non-state bodies may also be granted VAT exemption on the subject goods following an application submitted to the National Import Relief Unit.
In this context, goods must be imported and distributed free of charge to persons either at risk from or affected by COVID-19 as well as persons combating it the COVID-19 outbreak."

## GTA sources
covid.data$source[covid.data$entry.id==79078]="Council of the Eurasian Economic Commission. Decision N31 dated 3 April 2020, (On the draft resolution of the Eurasian Intergovernmental Council On measures taken within the framework of the Eurasian Economic Union aimed at ensuring economic stability in the context of the development of the COVID-19 coronavirus infection pandemic), https://docs.eaeunion.org/docs/ru-ru/01425315/err_06042020_31, http://www.eurasiancommission.org/ru/nae/news/Pages/03-04-2020-2."
covid.data$source[covid.data$entry.id==79234]="Decree of the President of Azerbaijan N974 dated 27 March 2020, http://e-qanun.az/framework/44801"
covid.data$source[covid.data$entry.id==79348]="Azerbaijan Cabinet of Ministers, Decrees, [Amendment in the import and export tariffs No.114], https://cabmin.gov.az/az/document/4350/"
covid.data$source[covid.data$entry.id==79351]="Azerbaijan Cabinet of Ministers, Decrees, [Decision on prohibition of certain medical products and preparations outside the country. Resolution No.42], https://cabmin.gov.az/az/document/4269/"
covid.data$source[covid.data$entry.id==79352]="Azerbaijan Cabinet of Ministers, Decrees, [Amendment in the import and export tariffs. Resolution No.84], https://cabmin.gov.az/az/document/4317/"
covid.data$source[covid.data$entry.id==79360]="Azerbaijan Cabinet of Ministers, Decrees, [Amendment in the import and export tariffs. Resolution No.141], https://cabmin.gov.az/az/document/4384/"
covid.data$source[covid.data$entry.id==12542]="Trade Notice: 03/ 2020-21 (15 April 2020), https://dgft.gov.in/sites/default/files/Trade%20Notice%2003%2015%20April%202020.pdf"
covid.data$source[covid.data$entry.id==79355]="Cabinet of Ministers of Ukraine. DECREE dated March 20, 2020, No. 224, Approving the List of Medicines, Medical Devices and / or Medical Equipment Required for the Implementation of Measures to Prevent the Occurrence and Dissemination, Localization and Elimination of Outbreaks, Epidemics and Pandemics of Coronavirus Disease (COVID-19) Exempt from Import Duty and the value added tax, https://www.kmu.gov.ua/npas/pro-zatverdzhennya-pereliku-likarskih-zasobiv-medichnih-virobiv-taabo-224200320?fbclid=IwAR0o1DLaCJxix3Ed5jdVDuUu--FvFHi3GVcv3q3GL4F32xR_q0tO0NDIl5w"


covid.data$source[covid.data$entry.id==3754]="Resolution Minist?rio da Sa?de/Ag?ncia Nacional de Vigil?ncia Sanit?ria Nos. 352, 370 and 371"
covid.data$source[covid.data$entry.id==3755]="Not?cia Siscomex Nos. 23/2020 and 12/2020; and Gecex/Camex Resolution Nos. 17/2020 and 31/2020"
covid.data$source[covid.data$entry.id==3089]="Permanent Delegation of the Dominican Republic to the WTO (1 May 2020)."
covid.data$source[covid.data$entry.id==2886]="WTO document G/MA/QR/N/EGY/1/Rev.1, 28 April 2020."
covid.data$source[covid.data$entry.id==2266]="Permanent Delegation of the Russian Federation to the WTO (20 April 2020) and Decision of the Council of the Eurasian Economic Commission No. 21 (16 March 2020)."
covid.data$source[covid.data$entry.id==2267]="Permanent Delegation of the Russian Federation to the WTO (20 April 2020) and Decision of the Council of the Eurasian Economic Commission No. 41 (24 March 2020)."
covid.data$source[covid.data$entry.id==2268]="Permanent Delegation of the Russian Federation to the WTO (20 April 2020) and Decision of the Council of the Eurasian Economic Commission No. 43 (31 March 2020)."
covid.data$source[covid.data$entry.id==2269]="Permanent Delegation of the Russian Federation to the WTO (20 April 2020) and Decision of the Council of the Eurasian Economic Commission Nos. 33 and 34 (3 April 2020)."

covid.data$source=gsub("\\s{2,}"," ",gsub("\\?+.\\?+","",covid.data$source))
  
## source attribution
unique(ricardo.data$acting.agency[ricardo.data$hint.id %in% covid.data$hint.id])
covid.data$source[covid.data$hint.id %in% subset(ricardo.data, grepl("WTO",acting.agency))$hint.id]=paste0("As also reported by the WTO; ",covid.data$source[covid.data$hint.id %in% subset(ricardo.data, grepl("WTO",acting.agency))$hint.id])
covid.data$source[covid.data$hint.id %in% subset(ricardo.data, grepl("ITC",acting.agency))$hint.id]=paste0("As also reported by the ITC; ",covid.data$source[covid.data$hint.id %in% subset(ricardo.data, grepl("ITC",acting.agency))$hint.id])
covid.data$source[covid.data$hint.id %in% subset(ricardo.data, grepl("WCO",acting.agency))$hint.id]=paste0("As also reported by the WCO; ",covid.data$source[covid.data$hint.id %in% subset(ricardo.data, grepl("WCO",acting.agency))$hint.id])
covid.data$source[covid.data$hint.id %in% subset(ricardo.data, grepl("YALE",acting.agency))$hint.id]=paste0("As also reported by the YALE University; ",covid.data$source[covid.data$hint.id %in% subset(ricardo.data, grepl("YALE",acting.agency))$hint.id])
covid.data$source[covid.data$hint.id %in% subset(ricardo.data, grepl("US-CRS",acting.agency))$hint.id]=paste0("As also reported by the US CRS; ",covid.data$source[covid.data$hint.id %in% subset(ricardo.data, grepl("US-CRS",acting.agency))$hint.id])


## ad-hoc collections until properly incorporated
adhoc.collections=c(2254, 2092, 504, 500, 2183, 2889, 2086, 2088, 2885, 3154, 2633)
covid.data=subset(covid.data, !hint.id %in% adhoc.collections)


## GTA entry
covid.data$entry.type[covid.data$hint.id==418]="Official source"

covid.data$entry.id=covid.data$hint.id
covid.data$hint.id=NULL


## remove partial removals
partial.removals=c(78930, 78979,79529, 5873)
covid.data=subset(covid.data, ! entry.id %in% partial.removals)

covid.data=unique(covid.data)
save(covid.data, file=paste0(project.path, "/data/GTA-COVID data.Rdata"))
save(covid.data, file=paste0(project.path, "/data/GTA-COVID data - ",Sys.Date(),".Rdata"))




## wb data
confirmed.0507=c(63, 2050, 3148, 224, 2074, 334, 2075, 355, 2078, 372, 2079, 2080, 2081, 418, 419, 445, 447, 454, 460, 475, 486, 491, 494, 495, 499, 500, 2084, 504, 2085, 508, 518, 522, 526, 541, 543, 547, 2086, 2087, 556, 558, 561, 564, 566, 567, 2088, 572, 577, 587, 589, 602, 606, 607, 609, 610, 617, 638, 2092, 2093, 2094, 2095, 644, 652, 2097, 657, 673, 727, 997, 2183, 3145, 1003, 1004, 1005, 1007, 1008, 1011, 1015, 1016, 1019, 1020, 1021, 2195, 2195, 2195, 2195, 2195, 2198, 1033, 1076, 2206, 1085, 2226, 1090, 1091, 1095, 1102, 2235, 2236, 2237, 2249, 1108, 1113, 1120, 1140, 1141, 1142, 1143, 2254, 2255, 2256, 2258, 2259, 2260, 2261, 2262, 2263, 2266, 2267, 2268, 2269, 2271, 2272, 2273, 2274, 2275, 2276, 2278, 2630, 2632, 2633, 2635, 3151, 2638, 3154, 2641, 2664, 2666, 2670, 2672, 2679, 2684, 2688, 2692, 3158, 2695, 2700, 2702, 2703, 2884, 2885, 2886, 2887, 2889, 2908, 2996, 3088, 3089, 3100, 3105, 3112, 3113, 3233, 3234, 3754, 3755, 3762, 3767, 3771, 12542, 58887, 59221, 61414, 73322, 73749, 73750, 73754, 78649, 78797, 78798, 78802, 78807, 78813, 78814, 78816, 78821, 78822, 78824, 78825, 78830, 78831, 78834, 78835, 78836, 78838, 78840, 78841, 78848, 78849, 78854, 78855, 78856, 78863, 78864, 78865, 78866, 78867, 78868, 78869, 78870, 78871, 78875, 78876, 78878, 78880, 78883, 78889, 78892, 78894, 78896, 78897, 78899, 78907, 78910, 78911, 78912, 78914, 78916, 78921, 78922, 78925, 78926, 78928, 78929, 78930, 78931, 78932, 78941, 78942, 78943, 78944, 78945, 78946, 78947, 78948, 78953, 78955, 78957, 78965, 78967, 78968, 78969, 78970, 78973, 78979, 78980, 78981, 78982, 78986, 78987, 78988, 78990, 78991, 78992, 78993, 78995, 79000, 79007, 79008, 79012, 79018, 79020, 79022, 79023, 79024, 79025, 79026, 79027, 79028, 79029, 79032, 79039, 79042, 79043, 79045, 79051, 79052, 79062, 79063, 79070, 79078, 79079, 79081, 79082, 79083, 79084, 79085, 79086, 79093, 79094, 79095, 79099, 79124, 79125, 79133, 79137, 79140, 79141, 79142, 79144, 79147, 79164, 79169, 79172, 79173, 79175, 79177, 79179, 79182, 79183, 79189, 79191, 79192, 79193, 79194, 79219, 79220, 79222, 79226, 79228, 79229, 79230, 79231, 79232, 79233, 79234, 79238, 79240, 79241, 79242, 79243, 79245, 79246, 79247, 79250, 79251, 79252, 79253, 79255, 79256, 79264, 79265, 79268, 79269, 79274, 79275, 79277, 79283, 79284, 79285, 79288, 79291, 79294, 79295, 79296, 79297, 79298, 79299, 79301, 79304, 79306, 79307, 79308, 79309, 79310, 79311, 79312, 79313, 79314, 79316, 79319, 79321, 79329, 79332, 79339, 79340, 79342, 79346, 79347, 79348, 79351, 79352, 79355, 79360, 79362, 79363, 79366, 79367, 79369, 79370, 79371, 79372, 79373, 79374, 79375, 79378, 79387, 79388, 79389, 79390, 79394, 79397, 79398, 79414, 79424, 2381, 2388, 2393, 3369, 3416)
confirmed.0514=c(3962, 3095, 2182, 2089, 2898, 3098, 3099, 4036, 4576, 2277, 4577, 3399, 3565, 3548, 4024, 3529, 2395, 3964, 3927, 1098, 2216, 3097, 2892, 
                 2203, 2264, 2257, 2207, 4051, 4050, 441, 2213, 3766, 4202, 2189, 4049, 3086, 3765, 4043, 3966, 3092, 3402, 4571, 2186, 2083, 1144, 2091, 408, 
                 4550, 3802, 3370, 3351, 3758, 3556, 4362, 3921, 2090, 3957, 3855, 3571, 3397, 3392, 3563, 79486, 79478, 79479, 79450, 4646, 4040, 2631, 79484, 
                 79483, 79485, 2970, 4628, 3011, 79489, 3400, 79482, 3012, 2634, 3932, 4727, 4365, 3760, 3763, 79514, 4025, 3413, 4110, 2202, 4044, 4023, 4220, 
                 4046, 2891, 4047, 3094, 79436, 4454, 79449, 79445, 79446, 79481, 3013, 79459, 79457, 79468, 79492, 79495, 2894, 2639, 3769, 3541, 79490, 79494,
                 79493, 2227, 79451, 3396, 3560, 3519, 4052, 79506, 79496, 79487, 79519, 79518, 79517, 79516, 3087, 659, 79151, 79327, 5563, 5545, 5873, 5516, 
                 2952, 5595, 5456, 2211, 2218, 79524, 4232, 6721, 79527, 79528, 6723, 79531, 6726, 79476, 79529)




# wb.data=subset(covid.data,( inst.export.barrier==T | inst.import.barrier==T) & (prd.med.any==T| prd.food==T ) & hint.id %in% checked.hints)
wb.data=subset(covid.data,( inst.export.barrier==T | inst.import.barrier==T) & (prd.med.any==T| prd.food==T ))
wb.data$inst.domestic.subsidy=NULL
wb.data$inst.export.subsidy=NULL
wb.data$inst.other=NULL
wb.data$inst.unclear=NULL
wb.data$inst.unkown=NULL
wb.data$prd.other=NULL
wb.data$intervention.type=NULL


wb.data$check.status="NEW"
wb.data$check.status[wb.data$entry.id %in% confirmed.0507]="checked for 7 May"
wb.data$check.status[wb.data$entry.id %in% confirmed.0514]="checked for 14 May"

table(wb.data$check.status)

names(wb.data)=c("Entry ID","Documentation status", "Jurisdiction","Initial assessment", "GTA intervention type", "Announcement date","Implementation date","Removal date", "Description", "Source", 
                 "Is export policy", "Is import policy","Product: medical consumables", "Product: Medical equipment","Product: Medicines or drugs", "Product: Food","Product: Any medical product", 
                 "Checking status")

# o column 2 and 3, so that the user can see if it is a food or medical product

## add collections
## extend attributes to all hints in collection
# 
# ADD SHEET: WITH VARIABLE KEYS
# ADD SHEET: README/HOWTOUSE

