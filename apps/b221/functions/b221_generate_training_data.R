b221_generate_training_data=function(only.gta.validators=T,
                                     save.path="17 Shiny/8 ricardo app/apps/b221/classifier"){

  library(gtasql)
  library(gtalibrary)
  library(pool)
  library(RMariaDB)
  library(data.table)
  library(gtabastiat)
  library(textutils)


  database <<- "ricardomain"

  gta_sql_pool_open(db.title=database,
                    db.host = gta_pwd(database)$host,
                    db.name = gta_pwd(database)$name,
                    db.user = gta_pwd(database)$user,
                    db.password = gta_pwd(database)$password,
                    table.prefix = "bt_")

  training.data=gta_sql_get_value("SELECT bht.hint_id, hint_title as act_title_en, hint_description as act_description_en, acting_agency, relevance, relevance_probability, relevance_accepted, gul.f_name as team_member, bcl.time_stamp as classification_time, gul2.f_name as validator
                                   FROM bt_hint_log bhl
                                   JOIN bt_hint_relevance bhr
                                   ON bhl.hint_id=bhr.hint_id
                                   JOIN bt_hint_text bht
                                   ON bhl.hint_id = bht.hint_id
                                   JOIN bt_classification_log bcl
                                   ON bhr.classification_id = bcl.classification_id
                                   JOIN bt_classification_log bcl2
                                   ON bhr.validation_classification = bcl2.classification_id
                                   JOIN gta_user_log gul
                                   ON bcl.user_id=gul.user_id
                                   JOIN gta_user_log gul2
                                   ON bcl2.user_id=gul2.user_id
                                   WHERE bhl.hint_state_id IN (3,4,5,6,7,9)
                                   AND bhr.relevance_accepted IS NOT NULL
                                   AND language_id=1
                                   AND gul.user_id!=70
                                   AND gul.user_id!=gul2.user_id;")

  ## setting evaluation to 1 for cases ultimately classifed as relevant (proposal =1 and accepted, or proposal =0 and not accepted)
  training.data$evaluation=0
  training.data$evaluation[training.data$relevance== training.data$relevance.accepted]=1

  training.data$text=training.data$act.title.en
  training.data$text[is.na(training.data$act.description.en)==F]=paste(training.data$text[is.na(training.data$act.description.en)==F], training.data$act.description.en[is.na(training.data$act.description.en)==F], sep=" ")
  training.data$act.title.en=NULL
  training.data$act.description.en=NULL


  ## encoding
  training.data$text=gsub("<.*?>","",iconv(training.data$text, "", "ASCII", "byte"))
  training.data$text=HTMLdecode(training.data$text)
  training.data$text=gsub("<.*?>", "", training.data$text)

  for(mbr in unique(training.data$team.member)){

    eval(parse(text=paste0("training.data$proposition.",tolower(gsub('\\W+','',mbr)),"='not involved'")))
    eval(parse(text=paste0("training.data$proposition.",tolower(gsub('\\W+','',mbr)),"[training.data$relevance==1 & training.data$team.member=='",mbr,"']='relevant'")))
    eval(parse(text=paste0("training.data$proposition.",tolower(gsub('\\W+','',mbr)),"[training.data$relevance==0 & training.data$team.member=='",mbr,"']='irrelevant'")))

  }

  ## creating date variable
  project.start="2020-04-21"
  training.data$project.week=round((as.numeric(as.Date(training.data$classification.time,  "%Y-%m-%d"))-as.numeric(as.Date(project.start,  "%Y-%m-%d")))/7, 0)

  if(only.gta.validators){
    true.validator=c("Anvar","Johannes", "Josse","Piotr", "Ana Elena","Simon", "Callum","Chintan","Halit", "Patrick", "Kamran")
    training.data=subset(training.data, validator %in% true.validator)
  }

  # cleaning up
  training.data$relevance=NULL
  training.data$relevance.accepted=NULL
  training.data$team.member=NULL
  training.data$classification.time=NULL
  training.data$validator=NULL
  save(training.data, file=paste0(save.path,"/training_data.Rdata"))

}
