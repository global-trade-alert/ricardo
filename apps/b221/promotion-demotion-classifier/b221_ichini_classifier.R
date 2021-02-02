b221_ichini_classifier = function(hint.vector,
                                  for.training = F,
                                  model.folder = ""){
  print('ichiniclassifier start')
  
  library(gtasql)
  library(stringr)
  
  #sometimes too many connections causing failure
  # init.db.connections = gta_sql_count_connections()
  
  #get the current wd in case it all goes wrong
  current.wd = getwd()
  
  tryCatch(
    expr = {
      
      #I tried to make Bastiat's functions work independently on their own
      
      #Quickly I realised this is a huge task. This can be done in the future if
      #required but will require a lot of refactoring.
      
      #For now, I think setting the wd like I have done here is a suitable stopgap.
      
      #load the most recent model and the tokeniser
      wd.pref = str_extract(getwd(), ".+GTA data team Dropbox")
      bastiat.wd = paste0(wd.pref, "/Bastiat")
      setwd(bastiat.wd)
      
      
      library(gtabastiat)
      library(pool)
# 
#       #open db connection
#       source("setup/keys/ric.R")
#       pool <<- pool::dbPool(
#         drv = RMySQL::MySQL(),
#         host = db.host,
#         username = db.user,
#         password = db.password,
#         dbname=db.name
#       )
#       rm(db.host, db.user, db.password, db.name)
#       session.prefix="bt_"

      #required for bastiat to work
      source("code/daily/infrastructure/Bastiat base.R")
      
      if(for.training){
        hint.vector = gta_sql_get_value("SELECT DISTINCT bthr.hint_id
                                    FROM bt_hint_relevance bthr
                                    WHERE bthr.relevance_accepted IS NOT NULL
                                    ORDER BY bthr.hint_id DESC
                                    LIMIT 20000")
        dbDisconnect(hint.vector)
        
      }
      
      
      
      # filter out hints that have fewer than 3 classifications
      # test only the hint_ids required
      
      select.hint.sql=paste("SELECT",hint.vector[1],"AS hint_id")
      if(length(hint.vector)>1){
        select.hint.sql = paste(select.hint.sql, 
                                paste(hint.vector[2:length(hint.vector)], collapse = " UNION SELECT "), 
                                sep =" UNION SELECT ")
      }
      
      
      #notice COUNT(...)>2 at the end
      hint.classify = gta_sql_get_value(query = paste0(
        "SELECT changed_hints.hint_id AS valid_hints
        FROM (", select.hint.sql,") changed_hints
        JOIN bt_hint_relevance bhr ON bhr.hint_id = changed_hints.hint_id AND bhr.validation_classification IS NULL
        JOIN bt_classification_log bcl ON bcl.classification_id = bhr.classification_id
        JOIN bt_hint_log bhl ON bhl.hint_id = changed_hints.hint_id
        GROUP BY bhr.hint_id HAVING COUNT(bcl.classification_id)>2;"))
      
      
      #only begin classification procedure if required
      if(!is.na(hint.classify)){
        
        
        if(is.na(hint.classify)){hint.classify = hint.vector}#for testing purposes. this line can't be reached IRL due to the !is.na() above.
        #if we don't put a value in hint.classify, the SQL below will fail.

        #not needed at the moment
        #hint.not.ready = hint.vector[-hint.classify]
        
        #generate a leads.core style df for Mrs H to use
        hint.classify.string = paste(hint.classify, collapse = ", ")
        leads.core = gta_sql_get_value(
          paste0(
            "SELECT DISTINCT btbid.hint_id, btbid.bid, bthl.acting_agency, btht.hint_title AS act_title_en, btht.hint_description AS act_description_en, bthl.hint_values, bthl.registration_date, gtajl.jurisdiction_name, bthl.hint_state_id
      FROM bt_hint_log bthl,
      	bt_hint_bid btbid,
      	bt_hint_text btht,
      	bt_hint_jurisdiction bthj,
      	gta_jurisdiction_list gtajl
      
      WHERE bthl.hint_id = btbid.hint_id
      AND bthl.hint_id = btht.hint_id
      AND bthl.hint_id = bthj.hint_id
      
      AND btbid.hint_id = bthj.hint_id
      AND btbid.hint_id = btht.hint_id
      
      AND bthj.hint_id = btht.hint_id
      
      AND bthj.jurisdiction_id = gtajl.jurisdiction_id
      
      AND btht.language_id = 1
      AND btbid.hint_id IN (", hint.classify.string,")"
          )
        )
        
        #crude way of removing duplicates
        leads.core=leads.core[!duplicated(leads.core$hint.id),]
        
        #send GN leads to Mrs Hudson for assessment
        leads.core.news = subset(leads.core, grepl("NEWS-", leads.core$bid))
        
        print('does it die here?')
        # K: It dies here
        if(nrow(leads.core.news)>0){
          leads.core.news$mrs.hudson.score = bt_estimate_news_leads(leads.core.news, binary.prediction = F)
        }
        print('no it does not')
        
        #prevent too many connections causing failure
        current.db.connections = gta_sql_count_connections()
        if(current.db.connections > 15){
          gta_sql_kill_connections(keep.x.first.connections = init.db.connections)
        }
        
        
        
        leads.core = merge(leads.core, leads.core.news, all.x = T)
        
        # K: make sure this if statement doesnt screw something else up
        if(nrow(leads.core)>0){ 
          #Mrs Hudson doesn't know how to assess non-news leads
          leads.core$mrs.hudson.score[is.na(leads.core$mrs.hudson.score)] = 1
        }
        
        # In comes Bastiat. He is not usually invoked on GN leads, hence why we need
        # to do so here to fill in relevance.probability for these hints.
        
        print("Bastiat start")
        
        #this is kept as similar to the structure in bt_leads_core_update() as
        #possible, so Bastiat can work with the kind of data he is used to.
        
        #However, some parts are not necessary so have been streamlined.
        
        ## classifying results
        classify=leads.core #=subset(leads.core, classify==1 & relevant==1 & country.lead!="Vatican")
        classify$text=paste(classify$act.title.en, classify$act.description.en, classify$hint.values, sep=" ")
        
        # removing non-ASCII
        classify$text=stringi::stri_trans_general(classify$text, "latin-ascii")
        classify$text=gsub("[^\001-\177]","",classify$text)
        
        classification.result=bt_squad_prediction(prediction.data.id=classify$bid,
                                                  prediction.data.text=classify$text,
                                                  prediction.acting.agency=classify$acting.agency)
        
        classify$relevant=NULL
        classify$relevance.probability=NULL
        classify$text=NULL
        
        classify=merge(classify, classification.result, by.x="bid",by.y="text.id", all.x = T)
        
        classified.bids=classify$bid
        #classified.lids=classify$lead.id
        #leads.core$bastiat.relevance=classify$relevant
        leads.core$bastiat.rel.prob=round(classify$relevance.probability,4)
        
        #fill in NAs
        leads.core$bastiat.rel.prob[is.na(leads.core$bastiat.rel.prob)] = 1
        
        
        
        #this isn't needed (at the moment)
        # ## checking for keywords
        # print("checking for negative keywords")
        # contains.negative.key=bt_classify_by_keyword(text.to.check=lc.update$act.title.en,
        #                                              text.id=lc.update$bid,
        #                                              flavour="negative")
        # 
        # if(any(contains.negative.key)){
        #   lc.update$relevant[contains.negative.key]=0
        
        print("Bastiat finish!")
        
        
        #prevent too many connections causing failure
        current.db.connections = gta_sql_count_connections()
        if(current.db.connections > 15){
          gta_sql_kill_connections(keep.x.first.connections = init.db.connections)
        }
        
        
        
        ###END BASTIAT CLASSIFICATION###
        
        
        ##### Begin freelancer relevance weighting calculation
        
        print("Begin freelancer relevance weighting calculation")
        
        hint.fl.relevance = gta_sql_get_value(paste0("SELECT bthr.hint_id, bthr.relevance, btcl.user_id
                                        FROM bt_hint_relevance bthr, bt_classification_log btcl
                                        WHERE bthr.classification_id = btcl.classification_id
                                        AND bthr.hint_id IN (", hint.classify.string, ")"))
        
        
        
        user.accuracy.score = b221_user_accuracy_rating()
        
        #tried to use an sapply() here but the for loop seems to be more stable and controllable.
        #calculate adjust FL relevance ratings based on their scores
        
        #this.user.accuracy = user.accuracy.score$user.accuracy[user.accuracy.score$user.id==hint.fl.relevance$user.id]
        
        for(i in 1:nrow(hint.fl.relevance)){
          this.user.accuracy = user.accuracy.score$user.accuracy[user.accuracy.score$user.id==hint.fl.relevance$user.id[i]]
          
          hint.fl.relevance$weighted.relevance[i] = ifelse(hint.fl.relevance$relevance[i]==0,
                                                           yes = 1 - this.user.accuracy,
                                                           no = this.user.accuracy)
        }
        
        hint.fl.relevance = hint.fl.relevance[!is.na(hint.fl.relevance$weighted.relevance),]
        
        #average out the weighted FL relevance scores per hint 
        fl.weighted.average.relevance = c()
        hint.ids.rlv = c()
        
        for(hint in unique(hint.fl.relevance$hint.id)){
          fl.weighted.average.relevance=c(fl.weighted.average.relevance, mean(hint.fl.relevance$weighted.relevance[hint.fl.relevance$hint.id == hint]))
          hint.ids.rlv = c(hint.ids.rlv, hint)
        }
        
        weighted.relevance.all = data.frame(hint.id = hint.ids.rlv,
                                            fl.weighted.average.relevance = fl.weighted.average.relevance)
        
        #removes some where the freelancer avg wasn't calculated properly
        leads.core = merge(leads.core, weighted.relevance.all)
        
        print("End freelancer weighting calculation")
        
        
        ####leads.core now prepared and ready to be classified####
        
        
        
        
        
        #if we are not doing training, call the classify function, which loads the saved model and classifies the leads.
        #confidence.threshold is the cutoff for probability
        #e.g if the model is 0.25 sure of relevance, and the cutoff is 0.3, this lead will be discarded.
        #if the confidence threshold is set to 0.2, it will be kept.
        #default is 0.2 to err on the side of caution.
        if(!for.training){
          
          leads.core$prediction = b221_ichini_estimate_hints(leads.core, model.folder = model.folder)#easier just to use model.folder = current.wd?
          
          #get the original vector of hint ids so the result is returned in the correct order
          result = data.frame(hint.id = hint.vector)
          
          #merge this with the predicted labels. relevant/promote = 1; irrelevant/demote = -1
          result=merge(result, leads.core[,c("hint.id", "prediction")], by = "hint.id", all.x = T)
          
          #ids that couldn't be classified stay in same state (this should be due to them not having 3 FL evaluations)
          result$prediction[is.na(result$prediction)] = 0
          
          print('ichiniclassifier terminated')
          #print(result)
          
          return(result)
        }
        
      }else{
        #if no hints eligible for classification, leave them alone
        return(data.frame(hint.id = hint.vector,
                          prediction = 0)
        )
      }
      
      
      
      
      # if we are training, let's get the correct labels for each lead
      
      training.hint.relevance = gta_sql_get_value("SELECT DISTINCT bthr.hint_id, bthr.relevance = bthr.relevance_accepted AS true_relevance
                                              FROM bt_hint_relevance bthr
                                              WHERE bthr.relevance_accepted IS NOT NULL
                                              ORDER BY bthr.hint_id DESC
                                              LIMIT 20000")
      
      leads.core = merge(leads.core, training.hint.relevance, all.x = T)
      
      leads.core = leads.core[!is.na(leads.core$true.relevance),]
      
      return(leads.core)
    },
    error = function(e.msg){
      message("Ichini classifier error:")
      message(traceback())
      print(e.msg)
    },
    # one of Bastiat's classifiers always generates a warning :)
    # warning = function(w.msg){
    #   message("Ichini classifier warning:")
    #   print(w.msg)
    # },
    finally = {
      setwd(current.wd)
      # gta_sql_kill_connections(keep.x.first.connections = init.db.connections) 
    }
  )
  
  
}




