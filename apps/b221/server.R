# Define reactive values used in this app module up here, these will be passed automatically
# Don't forget to add these variables as function parameters as well

# SERVER
b221server <- function(input, output, session, user, app, prm, ...) {
  
  # Set encoding for all tables, to make filtering in DT work
  gta_sql_get_value('SET NAMES utf8;')
  
  
  observe({
    delete.processing.ids <<- paste0("DELETE bt_hint_processing FROM bt_hint_processing
                                        JOIN bt_hint_log ON bt_hint_log.hint_id = bt_hint_processing.hint_id
                                        JOIN bt_hint_state_list ON bt_hint_log.hint_state_id = bt_hint_state_list.hint_state_id
                                        WHERE bt_hint_processing.user_id = ",user$id," AND (bt_hint_state_list.hint_state_name = 'B221 - freelancer desk' OR bt_hint_state_list.hint_state_name = 'B221 - editor desk' OR bt_hint_state_list.hint_state_name = 'trash bin - entered');") # freelancer and editor should be inverted in the if statement (just like this to test)
  })
  
  onStop(function(){
    cat("Performing application cleanup\n")
    gta_sql_multiple_queries(delete.processing.ids, output.queries = 1)
    # gta_sql_pool_close()
  })
  
  # Set reactive value to check whether first or second hint (state 3:7,9) is added to collection
  lockHint <- reactiveVal(FALSE)
  # Set reactive value to check to the first time opening slide in
  initial.slide.in <- reactiveVal(TRUE)
  # Create reactive List to track hint ids added to collection container
  hint.container <- reactiveValues(hint.ids=NULL, starred=NULL, official=NULL)
  
  # Track starred, official
  observeEvent(input$newStarred, {
    print("NEW STARRED")
    print(input$newStarred)
    hint.container$starred = input$newStarred
  })
  
  # Track removed Hints
  observeEvent(input$removeHint, {
    print("NEW REMOVED")
    print(input$removeHint)
    hint.container$hint.ids = c(hint.container$hint.ids[hint.container$hintd.ids != input$removeHint])
  })
  
  ns <- NS("b221")
  
  observe({
    # print(prm)
  })
  
  # CREATE DATAFRAMES FOR SELECTION FIELDS
  country.list <- gta_sql_get_value(sqlInterpolate(pool, "SELECT DISTINCT jurisdiction_name, jurisdiction_id FROM gta_jurisdiction_list;"))
  assessment.list <- gta_sql_get_value(sqlInterpolate(pool, "SELECT DISTINCT assessment_name, assessment_id FROM b221_assessment_list;"))
  product.list <- gta_sql_get_value(sqlInterpolate(pool, "SELECT DISTINCT product_group_name, product_group_id FROM b221_product_group_list;"))
  type.list <- gta_sql_get_value(sqlInterpolate(pool, "SELECT DISTINCT intervention_type_name, intervention_type_id FROM b221_intervention_type_list;"))
  
  # UPDATE DATE OF CREATION OF APP.R WHEN CLOSING, PREVENTS CACHING OF CSS AND JS
  onStop(function() {
    p <- paste0(getwd(), path, "/code/app.R")
    # Update file 'date creation'
    Sys.setFileTime(p, Sys.time())
  })
  
  # OUTPUT LEADS TABLE
  output$leadsTable <- DT::renderDataTable(DT::datatable(
    names(),
    rownames = FALSE,
    escape = FALSE,
    # USE TO SELECT WHEN SEARCH FIELD IS ACTIVE
    options = list(
      pageLength = 50,
      columnDefs = list(list(visible = FALSE, targets = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)), list(sortable=FALSE, targets = c(0)),
                        list(targets = c(24), render = JS("
                                                        function(data, type, row, meta){
                                                          return '';
                                                        }
                                                      "))),
      order = list(list(22, 'asc')),
      language = list(
        zeroRecords = "No more leads available."),
      rowCallback = JS("function ( row, data ) {
                            if (data[8]==null) {
                              var collection = '<div class=\\'noPartOfCollection collection-add no-touch\\'><img src=\\'www/b221/collection.svg\\' class=\\'svg no-touch\\'></div>'
                              var locked = '';
                            } else {
                              var collection = '<div id=\\'collection_'+data[8]+'\\' class=\\'partOfCollection collection-add no-touch\\'><img src=\\'www/b221/collection-added.svg\\' class=\\'svg no-touch\\'></div>'
                              var locked = ' locked';
                            }
                            if (data[3]==null) {
                              var description = 'No description available';
                            } else {
                              var description = data[3];
                            }
                            if (data[5]==null) {
                              var descriptionOriginal = '';
                              var toggleTranslate = '';
                            } else {
                              var descriptionOriginal = '<div class=\\'act-description-original\\'>'+data[5]+'</div>';
                              var toggleTranslate = '<span class=\\'material-icons translation-toggle\\'>translate</span>';
                            }
                            if (description.length > 450) {
                                var readMore = ' readMore';
                                var image = 'www/b221/expand.svg';
                            } else {
                                var readMore = '';
                                var image = 'www/b221/expandgrey.svg';
                            }
                            if (data[6]==null) {
                              var urlimage = '<div class=\\'background-url no-touch\\'><img src=\\'www/b221/urlgrey.svg\\' class=\\'svg no-touch\\'></div>';
                            } else {
                              var url = data[6];
                              var urlimage = '<div class=\\'background-url no-touch\\'><a class=\\'no-touch\\' href=\\''+url+'\\' target=\\'_blank\\'><img src=\\'www/b221/url.svg\\' class=\\'svg no-touch\\'></a></div>';
                            }
                            
                            let country = data[18];
                            let product = data[19];
                            let intervention = data[20];
                            let submit = '<div class=\\'submission\\'><button id=\\'submit_'+data[7]+'\\' type=\"button\" class=\"btn btn-default action-button\">Save changes</button></div>';
                            let checkboxCheck = data[14] == 1 ? ' checked' : '';
                            let official = '<div class=\\'is-official\\'><label for=\"official\">URL official</label><div class=\\'checkbox\\'><input type=\"checkbox\" id=\\'official_'+data[7]+'\\' name=\"official\" value=\"non-official\"'+checkboxCheck+'></div></div>';
                            let assessment = data[21];
                            let comment = data[22];
                            let dateAnnouncement = data[25];
                            let dateRegistration = data[26];
                            let dateRemoval = data[27];
                            
                            
                            var actingAgency = '<div class=\\'acting-agency\\'><label>Acting Agency</label><div class=\\'value\\'>'+data[1]+'</div></div>';
                            var title = '<div class=\\'title-row\\'><div class=\\'act-title\\'>'+data[2]+'</div></div>';
                            var descr = '<div class=\\'middle-row\\'><div class=\\'act-description\\'>'+description+'</div>'+descriptionOriginal+toggleTranslate+'<div class=\\'gradient-bottom\\'><div class=\\'gradient-inner\\'></div></div><div class=\\'show-more no-touch\\'><img src=\\''+image+'\\' class=\\'svg no-touch\\'></div></div>';
                            var buttons = '<div class=\\'bottom-row no-touch\\'>'+urlimage+collection+'</div>';
                            var options = '<div class=\\'top-row\\'>'+country+product+actingAgency+intervention+assessment+official+dateAnnouncement+dateRegistration+dateRemoval+'</div><div class=\\'comment\\'>'+comment+data[23]+'</div>';
                            var middle = '<div class=\\'middle-col'+readMore+'\\'>'+descr+'</div>';
                            var right = '<div class=\\'right-col\\'><div id=\\'discard\\' class=\\'evaluate\\'><span class=\\'material-icons\\'>cancel</span></div><div id=\\'relevant\\' class=\\'evaluate\\'><span class=\\'material-icons\\'>check_circle</span></div></div>';
                           $(row)
                           .append('<div id=\\'leadsID_'+data[7]+'\\' class=\\'leads-item'+locked+'\\'><div class=\\'left\\'>'+title+middle+options+buttons+submit+'</div><div class=\\'right\\'>'+right+'</div>')
                           .append('</div>');
                           return row; }"),
      initComplete = JS("function(settings) {
                          $('[id^=country]').selectize({
                            placeholder: 'Choose country...'
                          });
                          $('[id^=product]').selectize({
                            placeholder: 'Choose product...'
                          });
                          $('[id^=intervention]').selectize({
                            placeholder: 'Choose intervention type...'
                          });
                          $('[id^=assessment]').selectize({
                            placeholder: 'Choose assessment type...'
                          });
                          $('[id^=announcementdate] input').each((index, elem) => {
                          $(elem).bsDatepicker('update', $(elem)[0].getAttribute('data-initial-date')); })
                          $('[id^=implementationdate] input').each((index, elem) => {
                          $(elem).bsDatepicker('update', $(elem)[0].getAttribute('data-initial-date')); })
                          $('[id^=removaldate] input').each((index, elem) => {
                          $(elem).bsDatepicker('update', $(elem)[0].getAttribute('data-initial-date')); })
                          
                            
                          $('#b221-leadsTable').on('change','.shiny-date-input input',function (event) {
                            $(this.parentNode.parentNode.parentNode.parentNode).addClass('show-submission');
                          });
                          
                        }"),
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'), # reset
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }') # bind select boxes to Shiny
    ),
    callback = JS(paste0("table.on('click.dt','tr', function() {
                var data=table.row(this).data();
      }); hintsBasicUI(); submitSingleHint();",if(prm$autosubmit==1){"callLeadsDismiss(); checkLeads();"} else {"checkLeadsManual();"})),
    extensions = "Select",
    selection = "none"
  ),
  server = T)
  
  observe({
    print(paste0("table.on('click.dt','tr', function() {
                var data=table.row(this).data();
      }); hintsBasicUI(); submitSingleHint();",if(prm$autosubmit==1){"callLeadsDismiss(); checkLeads();"} else {"checkLeadsManual();"}))
  })
  
  
  # Main Table --------------------------------------------------------------
  
  # TABLE OUTPUT FOR PREDEFINED LIST OF WORDS
  names <- eventReactive(input$loadMoreLeads, {
    print("LEADS REFRESH")
    shinyjs::runjs("window.scrollTo(0, 0);")
    # if (prm$autosubmit == 0) {
    #   removeUI(selector = "#b221-submit")
    #   insertUI(selector = "#submitButton", ui = tagList(
    #     actionButton(ns("submit"), "Submit", class="green")
    #   ))
    # }
    
    # Pull parameters
    pull.group = ifelse(any(prm$freelancer == 1), "B221 - freelancer desk", "B221 - editor desk") # the if statement needs to be reversed when runs live
    
    # SQL INSERT HERE: USE KAMRAN'S PULL FUNCTIONS
    # THERE SHOULD BE A NEW COLUMN FOR COLLECTION IDS AS WELL, IF A HINT IS PART OF A COLLECTION, ELSE IT SHOULD BE NULL
    
    # KS: use bt_attribute_hint_processing(user.id = user.id, hint.state = NULL) where hint.state is for example 'B221 - freelancer desk'
    # bt_attribute_hint_processing(user.id = user$id, hint.state = "B221 - freelancer desk")
    # Then run to retrieve a dataframe to display: 
    # b221_pull_display_info(user.id = user$id, is.freelancer = T)
    # pull.group = "B221 - freelancer desk"
    # user = list('id'=40, "group"=2)
    
    print(paste0("FREELANCER: ",prm$freelancer))
    
    processing.hints = bt_attribute_hint_processing(user.id = user$id, hint.state = pull.group)
    
    if(is.character(processing.hints) & length(processing.hints) == 1){
      showNotification("No hints are available at this stage", duration = 5)
      leads.output <- data.frame()
    } else {
      pre.sorted.table <- b221_pull_display_info(user.id = user$id, is.freelancer = ifelse(prm$freelancer == 1, T, F)) # needs to be reversed when live, i put it opposite way for testing purposes
      pre.sorted.table$english.title = paste(pre.sorted.table$hint.id, pre.sorted.table$english.title, sep = ' - ')
      pre.sorted.table$original.description[!is.na(pre.sorted.table$original.description) & !is.na(pre.sorted.table$original.title)] = paste(pre.sorted.table$original.title[!is.na(pre.sorted.table$original.description) & !is.na(pre.sorted.table$original.title)],pre.sorted.table$original.description[!is.na(pre.sorted.table$original.description) & !is.na(pre.sorted.table$original.title)],sep='<br /><br />')
      leads.output <- pre.sorted.table[match(processing.hints,pre.sorted.table$hint.id),]
      rm('pre.sorted.table')
    }
    
    # Adding option fields for output (selected = x[] defines the value currently selected and will be defined by the data pull function)
    leads.output$select_country = apply(leads.output,1, function(x){
      as.character(selectizeInput(gsub(" ","",paste0('country_',x['hint.id'])),
                                  selected = unlist(strsplit(na.omit(as.character(x['jurisdiction.name'])), split=" ; ")), 
                                  label = 'Implementing country',
                                  choices = country.list$jurisdiction.name,
                                  multiple = TRUE)
      )
    })
    leads.output$select_product = apply(leads.output,1, function(x){
      as.character(selectizeInput(gsub(" ","",paste0('product_',x['hint.id'])),
                                  label = 'Product',
                                  selected = unlist(strsplit(na.omit(as.character(x['product.group.name'])), split=" ; ")),
                                  choices = product.list$product.group.name,
                                  multiple = TRUE)
      )
    })
    leads.output$select_intervention = apply(leads.output,1, function(x){
      as.character(selectizeInput(gsub(" ","",paste0('intervention_',x['hint.id'])),
                                  label = 'Intervention Type',
                                  selected = unlist(strsplit(na.omit(as.character(x['intervention.type.name'])), split=" ; ")),
                                  choices = type.list$intervention.type.name,
                                  multiple = TRUE)
      )
    })
    leads.output$select_assessment = apply(leads.output,1, function(x){
      as.character(selectizeInput(gsub(" ","",paste0('assessment_',x['hint.id'])),
                                  label = 'Assessment',
                                  selected = unlist(strsplit(na.omit(as.character(x['assessment.name'])), split=" ; ")),
                                  choices = assessment.list$assessment.name,
                                  multiple = FALSE)
      )
    })
    
    leads.output$new.comment = apply(leads.output,1, function(x){
      as.character(textInput(gsub(" ","",paste0('comment_',x['hint.id'])),
                             label = 'Comment',
                             placeholder = "Add new comment here...")
      )
    })
    
    # comments
    leads.output$comments = apply(leads.output,1, function(x){
      ifelse(is.na(x['comment']),
             yes = "",
             no = as.character(paste0("<div class='comment-list'>",paste0("<div class='comment-item'>",strsplit(x['comment'],split=" ; ")[[1]],"</div>",collapse=""),"</div>")))
    })
    
    if (nrow(leads.output)>0) {
      #order by date
      leads.output$order <- ifelse(is.na(leads.output$registration.date), yes = leads.output$hint.date, no = leads.output$registration.date)
      leads.output <- leads.output[with(leads.output, order(order, hint.date, decreasing = T)),]
      leads.output$order <- seq(1,nrow(leads.output),1)
      leads.output[,c("registration.date","hint.date")] <- NULL
    }
    
    # bring import barrier and export barrier to the top
    leads.output <- rbind(dplyr::filter(leads.output, grepl('import barrier|export barrier', intervention.type.name)),
                          dplyr::filter(leads.output, !grepl('import barrier|export barrier', intervention.type.name)))
    
    leads.output$order <- seq(1,nrow(leads.output),1)
    
    
    # Add Date fields
    leads.output$date_announcement = apply(leads.output,1, function(x){
      as.character(dateInput(gsub(" ","",paste0('announcementdate_',x['hint.id'])),
                             label = 'Announcement date',
                             value = x['announcement.date'], startview = Sys.Date(),
                             format = "yyyy-mm-dd")
      )
    })
    leads.output$date_registration = apply(leads.output,1, function(x){
      as.character(dateInput(gsub(" ","",paste0('implementationdate_',x['hint.id'])),
                             label = 'Implementation date',
                             value = x['implementation.date'], startview = Sys.Date(),
                             format = "yyyy-mm-dd")
      )
    })
    leads.output$date_removal = apply(leads.output,1, function(x){
      as.character(dateInput(gsub(" ","",paste0('removaldate_',x['hint.id'])),
                             label = 'Removal Date',
                             value = x['removal.date'], startview = Sys.Date(),
                             format = "yyyy-mm-dd")
      )
    })
    
    # bring import barrier and export barrier to the top
    leads.output <- rbind(dplyr::filter(leads.output, grepl('import barrier|export barrier', intervention.type.name)),
                          dplyr::filter(leads.output, !grepl('import barrier|export barrier', intervention.type.name)))
    
    leads.output$order <- seq(1,nrow(leads.output),1)
    
    leads.output <- leads.output
    leads.output <<- leads.output
  })
  
  
  # OBSERVE SHINY JS CHECK LEADS EVENT FOR ITEMS PASSING SCREEN TOP
  observeEvent(input$checkLeads, {
    id <- as.numeric(gsub("leadsID_","", input$checkLeads))
    print(paste0("Scrolled: ",id))
    if (prm$autosubmit == 1){ # check current state of the app
      
      # SQL INSERT HERE: SAVE DISMISSED STATUS FOR HINT ID
      # hint.id is served via id
      
      # OLD QUERY:
      # gta_sql_update_table(sqlInterpolate(pool, "UPDATE bt_leads_core SET bin_check = true, bin_recovered = false, relevant = false, sent_out = false, evaluation = false WHERE lead_id = ?leadsID;", leadsID = id))
      # gta_sql_update_table(sqlInterpolate(pool, "INSERT INTO bt_leads_checked VALUES (?leadsID, ?userID);", leadsID = id, userID = user$id))
    }
  })
  
  
  # OBSERVE CLICKS ON LEADS-ITEM AND CHANGE DATABSE ENTRY ACCORDINGLY
  observeEvent(input$checkLeadsClick, {
    # runjs(paste0("Shiny.unbindAll();"))
    id <- as.numeric(gsub("leadsID_","", input$checkLeadsClick[2]))
    print(paste0("Clicked: ",id))
    
    # SQL INSERT HERE: UPDATE HINT STATUS FOR DISMISSED HINT
    
    # gta_sql_update_table(sqlInterpolate(pool, "UPDATE bt_hint_log SET hint_state_id = 8 WHERE lead_id = ?leadsID;", leadsID = id))
    # b221_process_display_info(is.freelancer = ifelse(prm$freelancer == 1,1,0) ,user.id = user$id, processed.rows = changes) # freelancer editor is reversed
    
  })
  
  
  # Collection UI: Collection Button ----------------------------------------
  
  # OBSERVE CLICKS ON COLLECTION BUTTON AND OPEN SLIDEIN
  observeEvent(input$collectionAdd, {
    # runjs("Shiny.unbindAll($('#b221-collectionTable')[0]);")
    print(input$collectionAdd[3])
    removeUI(selector = ".removeslideinui",immediate = T)
    print("removingUI")
    hintId <- as.numeric(gsub("leadsID_","", input$collectionAdd[1]))
    currenthintId <<- hintId
    
    if (input$collectionAdd[3]=="TRUE") {
      collectionId <- as.numeric(gsub("collection_","", input$collectionAdd[2]))
      collection <- TRUE
    } else {
      collectionId <- FALSE
      collection <- FALSE
    }
    
    print("COLLETION ID")
    
    if (collection) {
      query = paste0("SELECT cltn_log.collection_id, cltn_log.collection_name, 
                        GROUP_CONCAT(DISTINCT(jur_list.jurisdiction_name) SEPARATOR ' ; ') AS jurisdiction_name,
                        GROUP_CONCAT(DISTINCT(ass_list.assessment_name) SEPARATOR ' ; ') AS assessment_name,
                        GROUP_CONCAT(DISTINCT(int_list.intervention_type_name) SEPARATOR ' ; ') AS intervention_type_name,
                        GROUP_CONCAT(DISTINCT(prod_grp_list.product_group_name) SEPARATOR ' ; ') AS product_group_name,
                        cltn_rel.relevance, cltn_star.hint_id AS starred_hint,
                        MAX(IF(bt_date_type_list.date_type_name='announcement', col_date.date, NULL )) AS announcement_date,
                        MAX(IF(bt_date_type_list.date_type_name='implementation', col_date.date, NULL )) AS implementation_date,
                        MAX(IF(bt_date_type_list.date_type_name='removal', col_date.date, NULL )) AS removal_date
                        FROM b221_collection_log cltn_log
                        JOIN b221_collection_jurisdiction cltn_jur ON cltn_jur.collection_id = cltn_log.collection_id AND cltn_log.collection_id = ",collectionId," JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_id = cltn_jur.jurisdiction_id
                        LEFT JOIN b221_collection_star cltn_star ON cltn_star.collection_id = cltn_log.collection_id
                        JOIN b221_collection_assessment cltn_ass ON cltn_ass.collection_id = cltn_log.collection_id JOIN b221_assessment_list ass_list ON cltn_ass.assessment_id = ass_list.assessment_id
                        JOIN b221_collection_intervention cltn_int ON cltn_int.collection_id = cltn_log.collection_id JOIN b221_intervention_type_list int_list ON int_list.intervention_type_id = cltn_int.intervention_type_id
                        JOIN b221_collection_product_group cltn_prod ON cltn_prod.collection_id = cltn_log.collection_id JOIN b221_product_group_list prod_grp_list ON prod_grp_list.product_group_id = cltn_prod.product_group_id
                        JOIN b221_collection_relevance cltn_rel ON cltn_rel.collection_id = cltn_log.collection_id
                        LEFT JOIN b221_collection_date col_date ON col_date.collection_id = cltn_log.collection_id LEFT JOIN bt_date_type_list ON col_date.date_type_id = bt_date_type_list.date_type_id
                        GROUP BY cltn_log.collection_id;")
      collectionStats <- gta_sql_get_value(query)
      
      # SQL INSERT HERE: CHECK IF HINT IS PART OF COLLECTION ALREADY, IF YES, GET COLLECTION VALUES, IF NO, GET HINT VALUES AND ADD TO initialXYZ VARIABLES
      
      initialName = collectionStats$collection.name
      initialPlaceholder = NULL
      initialProduct = unlist(na.omit(strsplit(collectionStats$product.group.name, " ; ")))
      initialType = unlist(na.omit(strsplit(collectionStats$intervention.type.name, " ; ")))
      initialAssessment = unlist(na.omit(strsplit(collectionStats$assessment.name, " ; ")))
      initialJurisdictions = unlist(na.omit(strsplit(collectionStats$jurisdiction.name, " ; ")))
      initialAnnouncement = unlist(na.omit(strsplit(collectionStats$announcement.date, " ; ")))
      initialImplementation = unlist(na.omit(strsplit(collectionStats$implementation.date, " ; ")))
      initialRemoval = unlist(na.omit(strsplit(collectionStats$removal.date, " ; ")))
      
      print(initialName)
      print(initialProduct)
      print(initialType)
      print(initialAssessment)
      print(initialJurisdictions)
      print(initialAnnouncement)
      print(initialImplementation)
      print(initialRemoval)
      
      # check if gta intervention amongst hints
      gtaHint <- FALSE
      initialHints <- get_info_by_collection_id(collection.id = collectionId)
      if (any(initialHints$is.intervention == 1)) {
        gtaHint <- TRUE
      }
      
      initialHints$hint.title <- paste(initialHints$hint.id, initialHints$hint.title, sep=" - ")
      
      initialHints$tpcontent = paste0('<div id="top-tooltip_',initialHints$hint.id,'" class="tipped-content"><div class="tipped-grid"">',
                                      '<div><label>Date</label>',initialHints$hint.date,'</div>',
                                      '<div><label>Acting Agency</label>',initialHints$acting.agency,'</div>',
                                      '<div><label>Implementer</label>',initialHints$jurisdiction.name,'</div>',
                                      '<div><label>Assessment</label>',initialHints$assessment,'</div>',
                                      '<div><label>Intervention type</label>',initialHints$intervention.type,'</div>',
                                      '<div><label>Product</label>',initialHints$product.group.name,'</div>',
                                      '</div><div class="tipped-description">',
                                      '<div><label>Description</label>',initialHints$hint.description,'</div>',
                                      '</div><div class="tipped-url">',
                                      '<div><label>URL official</label>',initialHints$official,'</div>',
                                      '<div><label>URL news</label>',initialHints$news,'</div>',
                                      '</div></div>')
      
      
      initialHints$url <- ifelse(is.na(initialHints$official), initialHints$news, initialHints$official)
      
      # update current hint.container reactiveVal
      hint.container$hint.ids <- c(unique(initialHints$hint.id))
      
      initialHints = generate_initial_hints(initialHints)
      
      slideInState = paste0("existingCollection_",collectionId)
      
      maxHint <- gta_sql_get_value(paste0("SELECT DISTINCT(hint_state_id) AS max_state FROM 
                                            (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collectionId,") ht_cltn 
                                            JOIN bt_hint_log ht_log ON ht_log.hint_id = ht_cltn.hint_id;"))
      
      locked <- ifelse(any((maxHint %in% c(3:7,9)) & prm$freelancer == 1) | gtaHint, " locked","")
      lockHint <- ifelse((any(maxHint %in% c(3:7,9)) & prm$freelancer == 1) | gtaHint, yes = lockHint(TRUE), no = lockHint(FALSE))
      
      
    } else {
      
      initialPlaceholder <- "Enter new Collection Name"
      initialName = NULL
      initialJurisdictions <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT jurisdiction_name FROM gta_jurisdiction_list WHERE jurisdiction_id IN (SELECT jurisdiction_id FROM bt_hint_jurisdiction WHERE (hint_id = ",hintId," AND classification_id = (SELECT MAX(classification_id) FROM bt_hint_jurisdiction WHERE hint_id = ",hintId,")));"))))
      initialProduct <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT product_group_name FROM b221_product_group_list WHERE product_group_id IN (SELECT product_group_id FROM b221_hint_product_group WHERE (hint_id = ",hintId," AND classification_id = (SELECT MAX(classification_id) FROM b221_hint_product_group WHERE hint_id = ",hintId,")));"))))
      initialType <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT intervention_type_name FROM b221_intervention_type_list WHERE intervention_type_id IN (SELECT apparent_intervention_id FROM b221_hint_intervention WHERE (hint_id = ",hintId," AND classification_id = (SELECT MAX(classification_id) FROM b221_hint_intervention WHERE hint_id = ",hintId,")));"))))
      initialAssessment <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT assessment_name FROM b221_assessment_list WHERE assessment_id IN (SELECT assessment_id FROM b221_hint_assessment WHERE (hint_id = ",hintId," AND classification_id = (SELECT MAX(classification_id) FROM b221_hint_assessment WHERE hint_id = ",hintId,")));"))))
      initialAnnouncement <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT MAX(IF(bt_date_type_list.date_type_name='announcement', bt_hint_date.date, NULL )) AS announcement_date FROM bt_hint_date  
LEFT JOIN bt_date_type_list ON bt_hint_date.date_type_id = bt_date_type_list.date_type_id WHERE bt_hint_date.hint_id = ",hintId,";"))))
      initialImplementation <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT MAX(IF(bt_date_type_list.date_type_name='implementation', bt_hint_date.date, NULL )) AS announcement_date FROM bt_hint_date  
LEFT JOIN bt_date_type_list ON bt_hint_date.date_type_id = bt_date_type_list.date_type_id WHERE bt_hint_date.hint_id = ",hintId,";"))))
      initialRemoval <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT MAX(IF(bt_date_type_list.date_type_name='removal', bt_hint_date.date, NULL )) AS announcement_date FROM bt_hint_date  
LEFT JOIN bt_date_type_list ON bt_hint_date.date_type_id = bt_date_type_list.date_type_id WHERE bt_hint_date.hint_id = ",hintId,";"))))
      
      initialHint <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT hint_id, hint_title 
                                                                          FROM bt_hint_text
                                                                          WHERE hint_id = ",hintId," AND language_id = 1;"))))
      
      
      # MOUSE OVER
      initSingleHint <- get_info_by_hint_id(hint.id = hintId)
      
      # initSingleHint = cbind(initSingleHint[,1:4],lapply(initSingleHint[,5:length(initSingleHint)], function(x) stri_trans_general(x, "Any-ascii")))
      initSingleHint = cbind(initSingleHint[,1:4],lapply(initSingleHint[,5:length(initSingleHint)], function(x) gsub("<.*?>","",iconv(x, "", "ASCII", "byte"))))
      
      initSingleHint$intervention.type <- gsub("export subsidy","Export subsidy",initSingleHint$intervention.type)
      initSingleHint$intervention.type <- gsub("domestic subsidy \\(incl\\. tax cuts, rescues etc\\.)","Domestic subsidy",initSingleHint$intervention.type)
      initSingleHint$intervention.type <- gsub("import barrier","Import barrier",initSingleHint$intervention.type)
      initSingleHint$intervention.type <- gsub("export barrier","Export barrier",initSingleHint$intervention.type)
      initSingleHint$intervention.type <- gsub("uncertain","Unclear",initSingleHint$intervention.type)
      
      initSingleHint$product.group.name <- gsub("uncertain","Uncertain",initSingleHint$product.group.name)
      initSingleHint$product.group.name <- gsub("medical consumables","Medical consumables",initSingleHint$product.group.name)
      initSingleHint$product.group.name <- gsub("medical equipment","Medical equipment",initSingleHint$product.group.name)
      initSingleHint$product.group.name <- gsub("medicines or drugs","Medicines or drugs",initSingleHint$product.group.name)
      initSingleHint$product.group.name <- gsub("food","Food",initSingleHint$product.group.name)
      initSingleHint$product.group.name <- gsub("other","Other",initSingleHint$product.group.name)
      
      initSingleHint$jurisdiction.name <- ifelse(is.na(initSingleHint$jurisdiction.name), "Unspecified", initSingleHint$jurisdiction.name)
      initSingleHint$intervention.type <- ifelse(is.na(initSingleHint$intervention.type), "Unspecified", initSingleHint$intervention.type)
      initSingleHint$product.group.name <- ifelse(is.na(initSingleHint$product.group.name), "Unspecified", initSingleHint$product.group.name)
      
      tpdate = paste0('<div><label>Date</label>',initSingleHint$hint.date,'</div>')
      tpimplementer = paste0('<div><label>Implementer</label>',initSingleHint$jurisdiction.name,'</div>')
      tpactingAgency = paste0('<div><label>Acting Agency</label>',initSingleHint$acting.agency,'</div>')
      tpassessment = paste0('<div><label>Assessment</label>',initSingleHint$assessment,'</div>')
      tpproduct = paste0('<div><label>Product</label>',initSingleHint$product.group.name,'</div>')
      tptype = paste0('<div><label>Intervention type</label>',initSingleHint$intervention.type,'</div>')
      tpofficial = paste0('<div><label>URL official</label>',initSingleHint$official,'</div>')
      tpnews = paste0('<div><label>URL news</label>',initSingleHint$news,'</div>')
      tpdescription = paste0('<div><label>Description</label>',initSingleHint$hint.description,'</div>')
      
      
      tpcontent = paste0('<div id="top-tooltip_',hintId,'" class="tipped-content"><div class="tipped-grid"">',tpdate,tpactingAgency,tpimplementer,tpassessment,tptype,tpproduct,'</div><div class="tipped-description">',tpdescription,'</div><div class="tipped-url">',tpofficial,tpnews,'</div></div>')
      
      initSingleHint$hint.title <- paste(initSingleHint$hint.id, initSingleHint$hint.title, sep=" - ")
      
      
      initSingleHint$url <- ifelse(is.na(initSingleHint$official), initSingleHint$news, initSingleHint$official)
      initSingleHint$tpcontent <- tpcontent
      
      initialHints <- generate_initial_hints(initSingleHint, initSingle = T)
      
      slideInState = "newCollection"
      
      locked = ""
      lockHint <- lockHint(FALSE)
      
      # Update hint.container reactiveVal
      hint.container$hint.ids <- c(hintId)
    }
    
    insertUI(immediate = T, selector = "#collectionValues",ui = tagList(
      tags$div(id=paste0("collectionContainer_",hintId),
               class="removeslideinui",
               tags$div(id=slideInState,
                        class="collectionHeader",
                        textInput(ns("newCollection"),
                                  label=NULL,
                                  value = initialName,
                                  placeholder=initialPlaceholder),
                        actionButton(ns("saveCollection"),
                                     label="Save",
                                     icon = icon("save"))),
               tags$div(class=paste0("initialValues",locked),
                        selectInput(ns("initImplementer"),
                                    label="Implementer",
                                    choices = country.list$jurisdiction.name,
                                    selected = initialJurisdictions,
                                    multiple = T),
                        selectInput(ns("initType"),
                                    label="Intervention Type",
                                    choices = type.list$intervention.type.name,
                                    selected = initialType,
                                    multiple = T),
                        selectInput(ns("initProduct"),
                                    label="Product",
                                    choices = product.list$product.group.name,
                                    selected = initialProduct,
                                    multiple = T),
                        selectInput(ns("initAssessment"),
                                    label="Assessment",
                                    choices = assessment.list$assessment.name,
                                    selected = initialAssessment,
                                    multiple = F),
                        dateInput(ns("initAnnouncement"),
                                  label="Announcement Date",
                                  value = initialAnnouncement, startview = Sys.Date(),
                                  format = "yyyy-mm-dd"),
                        dateInput(ns("initImplementation"),
                                  label="Implementation Date",
                                  value = initialImplementation, startview = Sys.Date(),
                                  format = "yyyy-mm-dd"),
                        dateInput(ns("initRemoval"),
                                  label="Removal Date",
                                  value = initialRemoval, startview = Sys.Date(),
                                  format = "yyyy-mm-dd")),
               tags$h4("Hints included"),
               tags$div(id="hintContainer",
                        HTML(initialHints)),
               tags$div(class="options-bar",
                        tags$button(id="discardCollection-popup",
                                    tags$i(class="fa fa-times"),
                                    "Discard"),
                        tags$div(id="confirm-discard",
                                 tags$div(class="confirm-discard-inner",
                                          tags$p("You are discarding a collection"),
                                          tags$div(class="button-wrap",
                                                   tags$button(class="cancel btn",
                                                               "Cancel"),
                                                   actionButton(ns("discardCollection"),
                                                                label="Discard",
                                                                icon = icon("times")))))))
    )
    )
    
    runjs("$('#b221-slideInRight').addClass('open');")
    runjs("$('#b221-slideInRight').trigger('loadCollectionSlideIn');console.log('2 loading collection slide in');")
    runjs("$('.tooltip-create-top').tooltipster({
                                theme: 'tooltipster-noir',
                                contentCloning: true,
                                maxWidth: 600,
                                arrow:false,
                                animationDuration: 150,
                                trigger: 'hover',
                                triggerOpen: {
                                    mouseenter: true
                                },
                                triggerClose: {
                                    click: true,
                                    scroll: true
                                }
                              })")
    runjs(paste0(" slideInBasicUI();"))
    
    
    if (initial.slide.in()) {
      runjs(paste0("markHints(); removeHint(); discardButton();"))
      initial.slide.in <- initial.slide.in(FALSE)
    }
    
    if (collection) {
      if (nrow(collectionStats)>0) {
        if (is.na(collectionStats$starred.hint)==F) {
          runjs(paste0("$('#hintContainer #hintId_",collectionStats$starred.hint,"').addClass('starred');"))
        }
      }
    }
    
  })
  
  observeEvent(input$saveCollection, {
    runjs('saveNewCollection();')
  })
  
  observeEvent(input$discardCollection, {
    runjs('discardExistingCollection();')
  })
  
  
  # Collection UI: Save Collection ------------------------------------------
  
  observeEvent(input$saveNewCollection, {
    colData <- jsonlite::fromJSON(input$saveNewCollection)
    
    # input <- list("newCollection"= 1,
    #                 "initImplementer"= c("United States of America","China"),
    #                 "initType"= "",
    #                 "initProduct"= "uncertain",
    #                 "initAssessment"= "unclear")
    # colData = list("collectionID"= "",
    #                "childIds"= 1)
    
    print(colData)  
    
    colName <- input$newCollection
    colImplementer <- input$initImplementer
    colType <- input$initType
    colProduct <- input$initProduct
    colAssessment <- input$initAssessment
    colAnnouncement <- input$initAnnouncement
    colImplementation <- input$initImplementation
    colRemoval <- input$initRemoval
    colHints <- as.numeric(colData$childIds)
    hintStarred <- ifelse(length(as.numeric(colData$starredHint))==0, NA, as.numeric(colData$starredHint))
    hintOfficial <- as.numeric(colData$officialHint)
    colState <- colData$state
    hintId <- as.numeric(gsub("collectionContainer_","",colData$hintId))
    
    colName <<- input$newCollection
    colImplementer <<- input$initImplementer
    colType <<- input$initType
    colProduct <<- input$initProduct
    colAssessment <<- input$initAssessment
    colAnnouncement <<- input$initAnnouncement
    colImplementation <<- input$initImplementation
    colRemoval <<- input$initRemoval
    colHints <<- as.numeric(colData$childIds)
    hintStarred <<- hintStarred
    hintOfficial <<- hintOfficial
    colState <<- colData$state
    hintId <<- as.numeric(gsub("collectionContainer_","",colData$hintId))
    
    print(colName)
    print(colImplementer)
    print(colType)
    print(colProduct)
    print(colAssessment)
    print(colAnnouncement)
    print(colImplementation)
    print(colRemoval)
    print(colHints)
    print(hintStarred)
    print(colState)
    print(hintId)
    
    
    if (any(nchar(colName)==0,
            nchar(colImplementer)==0,
            nchar(colType)==0, 
            nchar(colProduct)==0, 
            nchar(colAssessment)==0,
            is.null(colName),
            is.null(colImplementer),
            is.null(colType), 
            is.null(colProduct), 
            is.null(colAssessment),
            length(colName)==0,
            length(colImplementer)==0,
            length(colType)==0, 
            length(colProduct)==0, 
            length(colAssessment)==0)) {
      showNotification("Please fill out all necessary Values", duration = 3)
    } else {
      
      
      #map values
      colImplementerId = as.numeric(mapvalues(colImplementer, country.list$jurisdiction.name, country.list$jurisdiction.id, warn_missing = F))
      colTypeId = as.numeric(mapvalues(colType, type.list$intervention.type.name, type.list$intervention.type.id, warn_missing = F))
      colProductId = as.numeric(mapvalues(colProduct, product.list$product.group.name, product.list$product.group.id, warn_missing = F))
      colAssessmentId = as.numeric(mapvalues(colAssessment, assessment.list$assessment.name, assessment.list$assessment.id, warn_missing = F))
      
      
      # construct url DF
      hintUrls <- gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT * FROM
                                                                        (SELECT ht_log.hint_id, 
                                                                        GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='official', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS official,
                                                                        GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='news', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS news
                                                                        FROM bt_hint_log ht_log 
                                                                        JOIN bt_hint_url ht_url ON ht_url.hint_id = ht_log.hint_id JOIN bt_url_log ON ht_url.url_id = bt_url_log.url_id JOIN bt_url_type_list ON bt_url_type_list.url_type_id = ht_url.url_type_id
                                                                        WHERE ht_log.hint_id IN (",paste0(colHints, collapse=","),")
                                                                        GROUP BY ht_log.hint_id) unsorted_hints;")))
      
      
      hintUrls$url <- ifelse(is.na(hintUrls$official), hintUrls$news, hintUrls$official)
      hintUrls$is.official <- ifelse(hintUrls$hint.id %in% hintOfficial, 1, 0)
      hintUrls$official <- NULL
      hintUrls$news <- NULL
      
      
      
      
      # SQL INSERT HERE: UPDATE OR CREATE COLLECTION, 
      # IF NEW COLLECTION: colID == "newCollection"
      # IF UPDATING COLLECTION: colID == "collectionID_XXXX"
      
      collection.save = "successful"
      
      if (colState == "newCollection") {
        
        attributes = bt_find_collection_attributes(new.collection.name = colName, hints.id = colHints, starred.hint.id = hintStarred, country = colImplementerId, product = colProductId, intervention = colTypeId, assessment = colAssessmentId, relevance = 1, announcement.date = colAnnouncement, implementation.date = colImplementation, removal.date = colRemoval)
        
        collection.save =  b221_process_collections_hints(is.freelancer = ifelse(prm$freelancer == 1, T, F), 
                                                          user.id = user$id, 
                                                          new.collection.name = colName, 
                                                          hints.id = colHints, 
                                                          starred.hint.id = attributes$starred.hint.id, 
                                                          country = attributes$country, 
                                                          product = attributes$product, 
                                                          intervention = attributes$intervention, 
                                                          assessment = attributes$assessment, 
                                                          announcement.date = attributes$announcement.date, 
                                                          implementation.date = attributes$implementation.date, 
                                                          removal.date = attributes$removal.date, 
                                                          relevance = 1, 
                                                          collection.unchanged = F, 
                                                          empty.attributes = F)
        
        b221_hint_url(is.freelancer = ifelse(prm$freelancer == 1, T, F), user.id = user$id, hint.url.dataframe = hintUrls)
        
      } else {
        print('not new collection')
        
        collectionId <- as.numeric(gsub("existingCollection_","", colState))
        
        attributes = bt_find_collection_attributes(collection.id = collectionId, hints.id = colHints, starred.hint.id = hintStarred, country = colImplementerId, product = colProductId, intervention = colTypeId, assessment = colAssessmentId, relevance = 1, announcement.date = colAnnouncement, implementation.date = colImplementation, removal.date = colRemoval)
        
        cat(paste0("collection unchanged val: ",attributes$collection.unchanged,
                   "\ncollection impl val: ",paste0(attributes$country,collapse=','),
                   "\ncollection inttype val: ",paste0(attributes$intervention,collapse=','),
                   "\ncollection assessment val: ",paste0(attributes$assessment,collapse=','),
                   "\ncollection product val: ",paste0(attributes$product,collapse=',')))
        
        # is this hardcoded on purpose?
        b221_process_collections_hints(is.freelancer = F, 
                                       user.id = user$id, 
                                       collection.id = collectionId, 
                                       hints.id = colHints, 
                                       starred.hint.id = attributes$starred.hint.id, 
                                       country = attributes$country, 
                                       product = attributes$product, 
                                       intervention = attributes$intervention, 
                                       assessment = attributes$assessment, 
                                       announcement.date = attributes$announcement.date, 
                                       implementation.date = attributes$implementation.date, 
                                       removal.date = attributes$removal.date,
                                       relevance = 1, 
                                       collection.unchanged = attributes$collection.unchanged, 
                                       empty.attributes = F)
        
        b221_hint_url(is.freelancer = ifelse(prm$freelancer == 1, T, F), user.id = user$id, hint.url.dataframe = hintUrls)
        
        
      }
      
      print(paste0("THIS IS THE COUNTRY HINT ID country_",hintId))
      
      if (collection.save!='successful') {
        showNotification(collection.save, duration = 3)
      } else {
        
        # updateSelectInput(session = session, inputId = paste0("country_",hintId), selected = colImplementer)
        # updateSelectInput(session = session, inputId = paste0("product_",hintId), selected = colProduct)
        # updateSelectInput(session = session, inputId = paste0("intervention_",hintId), selected = colType)
        # updateSelectInput(session = session, inputId = paste0("assessment_",hintId), selected = colAssessment)
        # updateDateInput(session = session, inputId = paste0("announcementdate_",hintId), value = colAnnouncement)
        # updateDateInput(session = session, inputId = paste0("implementationdate_",hintId), value = colImplementation)
        # updateDateInput(session = session, inputId = paste0("removaldate_",hintId), value = colRemoval)
        
        collectionId <- gta_sql_get_value(paste0("SELECT collection_id FROM b221_hint_collection WHERE hint_id = ",hintId,";"))
        print(collectionId)
        if(is.na(collectionId)==F) {
          runjs(paste0("$('#leadsID_",hintId," .selectize-input .item').remove();"))
          insertCountry <- paste0('<div class="item" data-value=',colImplementer,'>',colImplementer,'</div>',collapse="")
          insertCountryOptions <- paste0('<option value=',colImplementer,' selected="selected">',colImplementer,'</option>')
          insertProduct <- paste0('<div class="item" data-value=',colProduct,'>',colProduct,'</div>',collapse="")
          insertProductOptions <- paste0('<option value=',colProduct,' selected="selected">',colProduct,'</option>')
          insertType <- paste0('<div class="item" data-value=',colType,'>',colType,'</div>',collapse="")
          insertTypeOptions <- paste0('<option value=',colType,' selected="selected">',colType,'</option>')
          insertAssessment <- paste0('<div class="item" data-value=',colAssessment,'>',colAssessment,'</div>',collapse="")
          insertAssessmentOptions <- paste0('<option value=',colAssessment,' selected="selected">',colAssessment,'</option>')
          
          runjs(paste0("$('select#country_",hintId,"').append('",insertCountryOptions,"');"))
          runjs(paste0("$('",insertCountry,"').hide().insertBefore('input#country_",hintId,"-selectized').fadeIn(300);"))
          runjs(paste0("$('select#product_",hintId,"').append('",insertProductOptions,"');"))
          runjs(paste0("$('",insertProduct,"').hide().insertBefore('input#product_",hintId,"-selectized').fadeIn(300);"))
          runjs(paste0("$('select#intervention_",hintId,"').append('",insertTypeOptions,"');"))
          runjs(paste0("$('",insertType,"').hide().insertBefore('input#intervention_",hintId,"-selectized').fadeIn(300);"))
          runjs(paste0("$('select#assessment_",hintId,"').append('",insertAssessmentOptions,"');"))
          runjs(paste0("$('",insertAssessment,"').hide().insertBefore('input#assessment_",hintId,"-selectized').fadeIn(300);"))
          runjs(paste0("$('#announcementdate_",hintId," input')[0].value = '",colAnnouncement,"';"))
          runjs(paste0("$('#implementationdate_",hintId," input')[0].value = '",colImplementation,"';"))
          runjs(paste0("$('#removaldate_",hintId," input')[0].value = '",colRemoval,"';"))
          runjs(paste0("$('#official_",hintId,"')[0].checked = ",ifelse(hintId %in% hintOfficial, "true", "false"),";"))
          
          runjs(paste0("$('#leadsID_",hintId,"').addClass('locked');"))
          runjs(paste0("$('#leadsID_",hintId,"').addClass('reactivate');"))
          runjs(paste0("$('#leadsID_",hintId," .collection-add').removeClass('noPartOfCollection');"))
          runjs(paste0("$('#leadsID_",hintId," .collection-add').addClass('partOfCollection');"))
          runjs(paste0("$('#leadsID_",hintId," .collection-add')[0].id = 'collection_",collectionId,"';"))
          runjs(paste0("$('#leadsID_",hintId," .collection-add img')[0].src = $('#leadsID_",hintId," .collection-add img')[0].src.replace('collection.svg', 'collection-added.svg');"))
        } else {
          runjs(paste0("$('#leadsID_",hintId," .collection-add').addClass('noPartOfCollection');"))
          runjs(paste0("$('#leadsID_",hintId," .collection-add').removeClass('partOfCollection');"))
          runjs(paste0("$('#leadsID_",hintId," .collection-add img')[0].src = $('#leadsID_",hintId," .collection-add img')[0].src.replace('collection-added.svg', 'collection.svg');"))
          runjs(paste0("$('#leadsID_",hintId," .collection-add')[0].id = '';"))
          runjs(paste0("$('#leadsID_",hintId,"').removeClass('locked');"))
          
        }
        runjs(paste0("$('#b221-slideInRight').removeClass('open');"))
        runjs(paste0("$('#b221-close-button').removeClass('open');"))
        runjs(paste0("$('.backdrop-nav').removeClass('open');"))
        removeUI(selector = ".removeslideinui",immediate = T)
        
      } 
      
    }
    
  })
  
  
  
  # Collection UI: Discard Collection ---------------------------------------
  
  # DISCARD EXISTING COLLECTION
  observeEvent(input$discardExistingCollection, {
    colData <- jsonlite::fromJSON(input$discardExistingCollection)
    print(colData)
    
    if (colData$state == "newCollection") {
      showNotification("This collection cannot be discarded, as it does not exist", duration = 3)
    } else {
      collectionId <- as.numeric(gsub("existingCollection_","", colData$state))
      print(paste0("THIS COLLECTION IS: ", collectionId))
      
      bt_delete_collection(collection.id=collectionId)
      
      runjs(paste0("$('#b221-slideInRight').removeClass('open');"))
      runjs(paste0("$('#b221-close-button').removeClass('open');"))
      runjs(paste0("$('.backdrop-nav').removeClass('open');"))
      removeUI(selector = ".removeslideinui",immediate = T)
    }
    
  })
  
  
  output$collectionTable <- DT::renderDataTable(DT::datatable(
    collections(),
    rownames = FALSE,
    escape = FALSE,
    # USE TO SELECT WHEN SEARCH FIELD IS ACTIVE
    options = list(
      pagingType = 'simple_numbers',
      pageLength = 10,
      columnDefs = list(list(visible = FALSE, targets = c(0,2,4:9)), list(sortable=FALSE, targets = c(0)),
                        list(targets = c(), render = JS("
                                                        function(data, type, row, meta){
                                                          return '';
                                                        }
                                                      ")),
                        list(title="Title", targets=c(1)),
                        list(title="Date", targets=c(3))),
      order = list(list(3, 'desc')),
      language = list(
        paginate = list("next"="<img src='www/b221/arrow_forward.svg'>", previous="<img src='www/b221/arrow_back.svg'>"),
        zeroRecords = "No more leads available.",
        search = "_INPUT_",
        searchPlaceholder = "Filter"),
      rowCallback = JS("function ( row, data ) {
        
                            let date = '<div class=\\'grid-row\\'><div class=\\'date tag\\'>'+data[3]+'</div></div>';
                            let assessment = '<div class=\\'grid-row\\'><div class=\\'assessment tag\\'>'+data[2]+'</div></div>';
                            let product = data[8];
                            let type = data[9];
                            let jurisdiction = data[7];
                            let collectionTitle = '<textarea id=\\'textCollection_'+data[0]+'\\' class=\\'textAreaCollection\\' type=\\'text\\'>'+data[1]+'</textarea>';
                            let collectionSave = '<button id=\\'renameCollection_'+data[0]+'\\' class=\\'renameCollection btn\\'>Save</button>'
                            
                            let tags = '<div class=\\'tags\\'>'+assessment+date+jurisdiction+type+product+'</div>';

                            let tpdate = '<div><label>Date</label>'+data[3]+'</div>';
                            let tpimplementer = '<div><label>Implementer</label>'+data[7]+'</div>';
                            let tpassessment = '<div><label>Assessment</label>'+data[2]+'</div>';
                            let tpproduct = '<div><label>Product</label>'+data[8]+'</div>';
                            let tptype = '<div><label>Intervention type</label>'+data[9]+'</div>';

                            let tpcontent = '<div id=\\'coltooltip_'+data[0]+'\\' class=\\'tipped-content\\'><div class=\\'tipped-grid\\'>'+tpdate+tpimplementer+tpassessment+tptype+tpproduct+'</div></div>';
                            
                            let tipped = '<span><span class=\\'material-icons\\'>info</span></span>';
                          
                           $(row)[0].innerHTML = '';
                           $(row)
                           .append('<div id=\\'collection_'+data[0]+'\\' class=\\'collection-item\\'><div class=\\'left\\'>'+tags+'<div class=\\'collection-title\\'>'+collectionTitle+collectionSave+'</div></div><div class=\\'right\\'><div data-tooltip-content=\\'#coltooltip_'+data[0]+'\\' class=\\'coltooltip-create info\\'>'+tipped+'</div><div id=\\'addCollection_'+data[0]+'\\' class=\\'icon addCollection\\'><span class=\\'material-icons add\\'>add_circle</span></div></div></div>'+tpcontent);

                           return row; }"),
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'), # reset
      drawCallback = JS("function() { Shiny.bindAll(this.api().table().node());
                              $('.coltooltip-create').tooltipster({
                                theme: 'tooltipster-noir',
                                contentCloning: true,
                                maxWidth: 600,
                                arrow:false,
                                animationDuration: 150,
                                trigger: 'hover',
                                triggerOpen: {
                                    mouseenter: true
                                },
                                triggerClose: {
                                    click: true,
                                    scroll: true
                                }
                              });
                              console.log('TOOLTIPS CREATED');
                            }")),
    callback = JS(""),
    selection = "none"
  ),
  server = F)
  
  
  
  
  # Collection UI: Collection Table -----------------------------------------
  
  # LOAD COLLECTIONS FOR COLLECTIONS SLIDE IN
  collections <- eventReactive(input$loadCollections, {
    
    runjs('console.log("test");')
    print("Collections refresh")
    # collectionsOutput <- gta_sql_get_value(sqlInterpolate(pool, "SELECT collection_id, collection_name FROM b221_collection_log;"))
    collectionsOutput <- gta_sql_get_value(sqlInterpolate(pool, "SELECT cltn.collection_id, cltn.collection_name, ass_list.assessment_name, MIN(bt_hint_log.hint_date) AS hint_date,
                                                                    GROUP_CONCAT(DISTINCT(int_list.intervention_type_name) SEPARATOR ' ; ') AS intervention_type_name,
                                                                    GROUP_CONCAT(DISTINCT(prod_list.product_group_name) SEPARATOR ' ; ') AS product_group_name,
                                                                    GROUP_CONCAT(DISTINCT(jur_list.jurisdiction_name) SEPARATOR ' ; ') AS jurisdiction_name
                                                                    FROM b221_collection_log cltn
                                                                    LEFT JOIN b221_collection_assessment cltn_ass ON cltn_ass.collection_id = cltn.collection_id LEFT JOIN b221_assessment_list ass_list ON cltn_ass.assessment_id = ass_list.assessment_id
                                                                    LEFT JOIN b221_collection_intervention cltn_int ON cltn_int.collection_id = cltn.collection_id LEFT JOIN b221_intervention_type_list int_list ON cltn_int.intervention_type_id = int_list.intervention_type_id
                                                                    LEFT JOIN b221_collection_product_group cltn_prod ON cltn_prod.collection_id = cltn.collection_id LEFT JOIN b221_product_group_list prod_list ON cltn_prod.product_group_id = prod_list.product_group_id
                                                                    LEFT JOIN b221_collection_jurisdiction cltn_jur ON cltn_jur.collection_id = cltn.collection_id LEFT JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_id = cltn_jur.jurisdiction_id
                                                                    LEFT JOIN b221_hint_collection ht_cltn ON ht_cltn.collection_id = cltn.collection_id LEFT JOIN bt_hint_log ON bt_hint_log.hint_id = ht_cltn.hint_id
                                                                    GROUP BY cltn.collection_id;"))
    # collectionsOutput[["collection.name"]] = stri_trans_general(collectionsOutput[["collection.name"]], "Any-ascii")
    collectionsOutput[["collection.name"]] = gsub("<.*?>","",iconv(collectionsOutput[["collection.name"]], "", "ASCII", "byte"))
    
    collectionsOutput$intervention.type.name <- gsub("export subsidy","Export subsidy", collectionsOutput$intervention.type.name)
    collectionsOutput$intervention.type.name <- gsub("domestic subsidy \\(incl\\. tax cuts, rescues etc\\.)","Domestic subsidy", collectionsOutput$intervention.type.name)
    collectionsOutput$intervention.type.name <- gsub("import barrier","Import barrier", collectionsOutput$intervention.type.name)
    collectionsOutput$intervention.type.name <- gsub("export barrier","Export barrier", collectionsOutput$intervention.type.name)
    collectionsOutput$intervention.type.name <- gsub("uncertain","Unclear", collectionsOutput$intervention.type.name)
    
    collectionsOutput$product.group.name <- gsub("uncertain","Uncertain",collectionsOutput$product.group.name)
    collectionsOutput$product.group.name <- gsub("medical consumables","Medical consumables",collectionsOutput$product.group.name)
    collectionsOutput$product.group.name <- gsub("medical equipment","Medical equipment",collectionsOutput$product.group.name)
    collectionsOutput$product.group.name <- gsub("medicines or drugs","Medicines or drugs",collectionsOutput$product.group.name)
    collectionsOutput$product.group.name <- gsub("food","Food",collectionsOutput$product.group.name)
    collectionsOutput$product.group.name <- gsub("other","Other",collectionsOutput$product.group.name)
    
    collectionsOutput$jurisdiction.name[is.na(collectionsOutput$jurisdiction.name)] <- "Unspecified"
    collectionsOutput$intervention.type.name[is.na(collectionsOutput$intervention.type.name)] <- "Unspecified"
    collectionsOutput$product.group.name[is.na(collectionsOutput$product.group.name)] <- "Unspecified"
    
    
    ## SORTING FOR RELEVANCE
    initialJurisdictions <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT jurisdiction_name FROM gta_jurisdiction_list WHERE jurisdiction_id IN (SELECT jurisdiction_id FROM bt_hint_jurisdiction WHERE hint_id = ",as.numeric(input$loadCollections),");"))))
    initialProduct <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT product_group_name FROM b221_product_group_list WHERE product_group_id IN (SELECT product_group_id FROM b221_hint_product_group WHERE hint_id = ",as.numeric(input$loadCollections),");"))))
    initialType <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT intervention_type_name FROM b221_intervention_type_list WHERE intervention_type_id IN (SELECT apparent_intervention_id FROM b221_hint_intervention WHERE hint_id = ",as.numeric(input$loadCollections),");"))))
    initialAssessment <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT assessment_name FROM b221_assessment_list WHERE assessment_id IN (SELECT assessment_id FROM b221_hint_assessment WHERE hint_id = ",as.numeric(input$loadCollections),");"))))
    
    initialJurisdictions <- unlist(strsplit(na.omit(as.character(initialJurisdictions)), " ; "))
    initialType <- unlist(strsplit(na.omit(as.character(initialType)), " ; "))
    initialProduct <- unlist(strsplit(na.omit(as.character(initialProduct)), " ; "))
    initialAssessment <- unlist(strsplit(na.omit(as.character(initialAssessment)), " ; "))
    
    initialJurisdictions <- ifelse(is.null(initialJurisdictions),character(0),initialJurisdictions)
    initialType <- ifelse(is.null(initialType),character(0),initialType)
    initialProduct <- ifelse(is.null(initialProduct),character(0),initialProduct)
    initialAssessment <- ifelse(is.null(initialAssessment),character(0),initialAssessment)
    
    initialDate <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT hint_date FROM bt_hint_log WHERE hint_id = ",as.numeric(input$loadCollections),";"))))
    
    # weight.jur=100
    # weight.int.type=1
    # weight.assessment=1
    # weight.date=.1
    # weight.product=1
    # 
    # collectionsOutput$order = as.vector(do.call(rbind, lapply(as.list(strsplit(collectionsOutput$intervention.type.name, split = ' ; ')), function(x) sum(x %in% initialType))) * weight.int.type+
    #                                       do.call(rbind, lapply(as.list(strsplit(collectionsOutput$jurisdiction.name, split = ' ; ')), function(x) sum(x %in% initialJurisdictions))) * weight.jur +
    #                                       do.call(rbind, lapply(as.list(strsplit(collectionsOutput$product.group.name, split = ' ; ')), function(x) sum(x %in% initialProduct))) * weight.product +
    #                                       do.call(rbind, lapply(collectionsOutput$assessment.name, function(x) sum(x %in% initialAssessment))) * weight.assessment+
    #                                       do.call(rbind, lapply(collectionsOutput$hint.date, function(x) log(1/(abs(as.numeric(as.Date(initialDate))-as.numeric(as.Date(x)))))))* weight.date  )
    # 
    # collectionsOutput$order[collectionsOutput$order<0]=0
    # 
    # collectionsOutput=collectionsOutput[order(collectionsOutput$order, decreasing = T),]
    
    
    ## generate HTML
    
    collectionsOutput$tag_country = apply(collectionsOutput,1, function(x){
      as.character(paste0("<div class='grid-row'>",paste0("<div class='tag country'>",substr(strsplit(x['jurisdiction.name'],split=" ; ")[[1]],1,20),"</div>",collapse=""),"</div>"))
    })
    collectionsOutput$tag_product = apply(collectionsOutput,1, function(x){
      as.character(paste0("<div class='grid-row'>",paste0("<div class='tag product'>",substr(strsplit(x['product.group.name'],split=" ; ")[[1]],1,20),"</div>",collapse=""),"</div>"))
    })
    collectionsOutput$tag_type = apply(collectionsOutput,1, function(x){
      as.character(paste0("<div class='grid-row'>",paste0("<div class='tag type'>",substr(strsplit(x['intervention.type.name'],split=" ; ")[[1]],1,20),"</div>",collapse=""),"</div>"))
    })
    
    # explicitly declare numeric columns
    collectionsOutput$collection.id <- as.numeric(collectionsOutput$collection.id)
    
    collectionsOutput <<- collectionsOutput
  })
  
  output$singleHintsTable <- DT::renderDataTable(DT::datatable(
    singleHints(),
    rownames = FALSE,
    escape = FALSE,
    # USE TO SELECT WHEN SEARCH FIELD IS ACTIVE
    options = list(
      pagingType = 'simple_numbers',
      pageLength = 20,
      columnDefs = list(list(visible = FALSE, targets = c(0:2,4,6:20)), list(sortable=FALSE, targets = c(0)),
                        list(targets = c(), render = JS("
                                                        function(data, type, row, meta){
                                                          return '';
                                                        }
                                                      ")),
                        list(title="Title", targets=c(5)),
                        list(title="Date", targets=c(3))),
      order = list(list(3, 'desc')),
      colnames = c("Title","Date"),
      language = list(
        paginate = list("next"="<img src='www/b221/arrow_forward.svg'>", previous="<img src='www/b221/arrow_back.svg'>"),
        zeroRecords = "No more leads available.",
        search = "_INPUT_",
        searchPlaceholder = "Filter"),
      rowCallback = JS("function ( row, data ) {
                            
                            if (! [2,8].includes(data[1]) && data[9] != null ) {
                            var lock = ' locked';
                            } else {
                            var lock = '';
                            } 
                            
                            let date = '<div class=\\'grid-row\\'><div class=\\'date tag\\'>'+data[3]+'</div></div>';
                            let assessment = '<div class=\\'grid-row\\'><div class=\\'assessment tag\\'>'+data[7]+'</div></div>';
                            let product = data[19];
                            let type = data[20];
                            let jurisdiction = data[18];
                            let intervention = data[17] == 1 ? '<img src=\\'www/b221/intervention.svg\\' class=\\'intervention-icon no-touch\\'>' : '';

                            
                            let tags = '<div class=\\'tags\\'>'+assessment+date+jurisdiction+type+product+'</div>';
                            let tags2 = '<div class=\\'tags-lower\\'>'+type+product+'</div>';
                            
                            let tpdate = '<div><label>Date</label>'+data[3]+'</div>';
                            let tpactingAgency = '<div><label>Acting Agency</label>'+data[2]+'</div>';
                            let tpimplementer = '<div><label>Implementer</label>'+data[4]+'</div>';
                            let tpdescription = '<div><label>Description</label>'+data[6]+'</div>';
                            let tpassessment = '<div><label>Assessment</label>'+data[7]+'</div>';
                            let tpproduct = '<div><label>Product</label>'+data[12]+'</div>';
                            let tptype = '<div><label>Intervention type</label>'+data[11]+'</div>';
                            let tpofficial = '<div><label>URL official</label>'+data[13]+'</div>';
                            let tpnews = '<div><label>URL news</label>'+data[14]+'</div>';
                            
                            let tpcontent = '<div id=\\'tooltip_'+data[0]+'\\' class=\\'tipped-content\\'><div class=\\'tipped-grid\\'>'+tpdate+tpactingAgency+tpimplementer+tpassessment+tptype+tpproduct+'</div><div class=\\'tipped-description\\'>'+tpdescription+'</div><div class=\\'tipped-url\\'>'+tpofficial+tpnews+'</div></div>';
                            
                            let tipped = '<span><span class=\\'material-icons\\'>info</span></span>';

                           $(row)[0].innerHTML = '';
                           $(row)
                           .append('<div id=\\'hint_'+data[0]+'\\' class=\\'hint-item'+lock+'\\'><div class=\\'left\\'>'+tags+'<div class=\\'hint-title\\'>'+intervention+data[5]+'</div></div><div class=\\'right\\'><div data-tooltip-content=\\'#tooltip_'+data[0]+'\\' class=\\'tooltip-create info\\'>'+tipped+'</div><div class=\\'icon\\'><span class=\\'material-icons lock\\'>lock</span><span class=\\'material-icons add\\'>add_circle</span></div></div></div>'+tpcontent);
                           return row; 
                            }"),
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'), # reset
      drawCallback = JS("function() { Shiny.bindAll(this.api().table().node());
                              $('.tooltip-create').tooltipster({
                                theme: 'tooltipster-noir',
                                contentCloning: true,
                                maxWidth: 600,
                                arrow:false,
                                animationDuration: 150,
                                trigger: 'hover',
                                triggerOpen: {
                                    mouseenter: true
                                },
                                triggerClose: {
                                    click: true,
                                    scroll: true
                                }
                              });
                              console.log('TOOLTIPS CREATED');
                            }")),
    callback = JS(""),
    extensions = "Select",
    selection = "single"
  ),
  server = F)
  
  
  # Collection UI: Single Hints Table ---------------------------------------
  
  # LOAD SINGLE HINTS FOR COLLECTIONS SLIDE IN
  singleHints <- eventReactive(input$loadSingleHints, {
    print("SingleHintRefresh refresh")
    print(user$id)
    singleHintOutput <- get_single_hints_infos(user.id = user$id)
    # singleHintOutput = cbind(singleHintOutput[,1:4],lapply(singleHintOutput[,5:length(singleHintOutput)], function(x) stri_trans_general(x, "Any-ascii")))
    singleHintOutput = cbind(singleHintOutput[,1:4],lapply(singleHintOutput[,5:length(singleHintOutput)], function(x) gsub("<.*?>","",iconv(x, "", "ASCII", "byte"))))
    singleHintOutput$hint.title <- paste(singleHintOutput$hint.id, singleHintOutput$hint.title, sep=" - ")
    
    singleHintOutput$intervention.type <- gsub("export subsidy","Export subsidy",singleHintOutput$intervention.type)
    singleHintOutput$intervention.type <- gsub("domestic subsidy \\(incl\\. tax cuts, rescues etc\\.)","Domestic subsidy",singleHintOutput$intervention.type)
    singleHintOutput$intervention.type <- gsub("import barrier","Import barrier",singleHintOutput$intervention.type)
    singleHintOutput$intervention.type <- gsub("export barrier","Export barrier",singleHintOutput$intervention.type)
    singleHintOutput$intervention.type <- gsub("uncertain","Unclear",singleHintOutput$intervention.type)
    
    singleHintOutput$product.group.name <- gsub("uncertain","Uncertain",singleHintOutput$product.group.name)
    singleHintOutput$product.group.name <- gsub("medical consumables","Medical consumables",singleHintOutput$product.group.name)
    singleHintOutput$product.group.name <- gsub("medical equipment","Medical equipment",singleHintOutput$product.group.name)
    singleHintOutput$product.group.name <- gsub("medicines or drugs","Medicines or drugs",singleHintOutput$product.group.name)
    singleHintOutput$product.group.name <- gsub("food","Food",singleHintOutput$product.group.name)
    singleHintOutput$product.group.name <- gsub("other","Other",singleHintOutput$product.group.name)
    
    singleHintOutput$jurisdiction.name[is.na(singleHintOutput$jurisdiction.name)] <- "Unspecified"
    singleHintOutput$intervention.type[is.na(singleHintOutput$intervention.type)] <- "Unspecified"
    singleHintOutput$product.group.name[is.na(singleHintOutput$product.group.name)] <- "Unspecified"
    
    # removing active hint
    singleHintOutput=subset(singleHintOutput, ! hint.id %in% as.numeric(input$loadCollections))
    
    ht_val = as.numeric(gsub("leadsID_| ","",input$loadSingleHints))
    
    ### SORTING FOR RELEVANCE
    initialJurisdictions <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT jurisdiction_name FROM gta_jurisdiction_list WHERE jurisdiction_id IN (SELECT jurisdiction_id FROM bt_hint_jurisdiction WHERE hint_id = ",ht_val,");"))))
    initialProduct <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT product_group_name FROM b221_product_group_list WHERE product_group_id IN (SELECT product_group_id FROM b221_hint_product_group WHERE hint_id = ",ht_val,");"))))
    initialType <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT intervention_type_name FROM b221_intervention_type_list WHERE intervention_type_id IN (SELECT apparent_intervention_id FROM b221_hint_intervention WHERE hint_id = ",ht_val,");"))))
    initialAssessment <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT assessment_name FROM b221_assessment_list WHERE assessment_id IN (SELECT assessment_id FROM b221_hint_assessment WHERE hint_id = ",ht_val,");"))))
    
    initialJurisdictions <- unlist(strsplit(na.omit(as.character(initialJurisdictions)), " ; "))
    initialType <- unlist(strsplit(na.omit(as.character(initialType)), " ; "))
    initialProduct <- unlist(strsplit(na.omit(as.character(initialProduct)), " ; "))
    initialAssessment <- unlist(strsplit(na.omit(as.character(initialAssessment)), " ; "))
    
    initialJurisdictions <- ifelse(is.null(initialJurisdictions),character(0),initialJurisdictions)
    initialType <- ifelse(is.null(initialType),character(0),initialType)
    initialProduct <- ifelse(is.null(initialProduct),character(0),initialProduct)
    initialAssessment <- ifelse(is.null(initialAssessment),character(0),initialAssessment)
    
    initialDate <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT hint_date FROM bt_hint_log WHERE hint_id = ",ht_val,";"))))
    
    # weight.jur=100
    # weight.int.type=1
    # weight.assessment=1
    # weight.date=.1
    # weight.product=1
    # 
    # singleHintOutput$order = as.vector(do.call(rbind, lapply(as.list(strsplit(as.character(singleHintOutput$intervention.type), split = ' ; ')), function(x) sum(x %in% initialType))) * weight.int.type+
    #                                      do.call(rbind, lapply(as.list(strsplit(as.character(singleHintOutput$jurisdiction.name), split = ' ; ')), function(x) sum(x %in% initialJurisdictions))) * weight.jur +
    #                                      do.call(rbind, lapply(as.list(strsplit(as.character(singleHintOutput$product.group.name), split = ' ; ')), function(x) sum(x %in% initialProduct))) * weight.product +
    #                                      do.call(rbind, lapply(singleHintOutput$assessment.name, function(x) sum(x %in% initialAssessment))) * weight.assessment+
    #                                      do.call(rbind, lapply(singleHintOutput$hint.date, function(x) log(1/(abs(as.numeric(as.Date(initialDate))-as.numeric(as.Date(x)))))))* weight.date  )
    # 
    # singleHintOutput$order[singleHintOutput$order<0]=0
    # 
    # singleHintOutput=singleHintOutput[order(singleHintOutput$order, decreasing = T),]
    
    
    ## generate HTML
    
    singleHintOutput$tag_country = apply(singleHintOutput,1, function(x){
      as.character(paste0("<div class='grid-row'>",paste0("<div class='tag country'>",substr(strsplit(x['jurisdiction.name'],split=" ; ")[[1]],1,20),"</div>",collapse=""),"</div>"))
    })
    singleHintOutput$tag_product = apply(singleHintOutput,1, function(x){
      as.character(paste0("<div class='grid-row'>",paste0("<div class='tag product'>",substr(strsplit(x['product.group.name'],split=" ; ")[[1]],1,20),"</div>",collapse=""),"</div>"))
    })
    singleHintOutput$tag_type = apply(singleHintOutput,1, function(x){
      as.character(paste0("<div class='grid-row'>",paste0("<div class='tag type'>",substr(strsplit(x['intervention.type'],split=" ; ")[[1]],1,20),"</div>",collapse=""),"</div>"))
    })
    
    print("LENGTH OF SINGLE HINT OUTPUT")
    
    # explicitly declare numeric columns
    singleHintOutput$hint.id <- as.numeric(singleHintOutput$hint.id)
    singleHintOutput$hint.state.id <- as.numeric(singleHintOutput$hint.state.id)
    singleHintOutput$prio.cty <- as.character(singleHintOutput$prio.cty)
    singleHintOutput$collection.id <- as.numeric(singleHintOutput$collection.id)
    
    print(nrow(singleHintOutput))
    singleHintOutput <<- singleHintOutput
  })
  
  
  # Collection UI: Select Single Hints  -------------------------------------
  
  # SELECT ROWS MECHANISM HITNS TABLE
  observeEvent(input$singleHintsTable_rows_selected, { 
    moveHint <- singleHintOutput[input$singleHintsTable_rows_selected,]
    rowtest <<- moveHint
    print(moveHint)
    
    # MOUSE OVER
    initSingleHint <- get_info_by_hint_id(hint.id = moveHint$hint.id)
    
    initSingleHint$intervention.type <- gsub("export subsidy","Export subsidy",initSingleHint$intervention.type)
    initSingleHint$intervention.type <- gsub("domestic subsidy \\(incl\\. tax cuts, rescues etc\\.)","Domestic subsidy",initSingleHint$intervention.type)
    initSingleHint$intervention.type <- gsub("import barrier","Import barrier",initSingleHint$intervention.type)
    initSingleHint$intervention.type <- gsub("export barrier","Export barrier",initSingleHint$intervention.type)
    initSingleHint$intervention.type <- gsub("uncertain","Unclear",initSingleHint$intervention.type)
    
    initSingleHint$product.group.name <- gsub("uncertain","Uncertain",initSingleHint$product.group.name)
    initSingleHint$product.group.name <- gsub("medical consumables","Medical consumables",initSingleHint$product.group.name)
    initSingleHint$product.group.name <- gsub("medical equipment","Medical equipment",initSingleHint$product.group.name)
    initSingleHint$product.group.name <- gsub("medicines or drugs","Medicines or drugs",initSingleHint$product.group.name)
    initSingleHint$product.group.name <- gsub("food","Food",initSingleHint$product.group.name)
    initSingleHint$product.group.name <- gsub("other","Other",initSingleHint$product.group.name)
    
    initSingleHint$jurisdiction.name[is.na(initSingleHint$jurisdiction.name)] <- "Unspecified"
    initSingleHint$intervention.type[is.na(initSingleHint$intervention.type)] <- "Unspecified"
    initSingleHint$product.group.name[is.na(initSingleHint$product.group.name)] <- "Unspecified"
    
    tpdate = paste0('<div><label>Date</label>',initSingleHint$hint.date,'</div>')
    tpimplementer = paste0('<div><label>Implementer</label>',initSingleHint$jurisdiction.name,'</div>')
    tpactingAgency = paste0('<div><label>Acting Agency</label>',initSingleHint$acting.agency,'</div>')
    tpassessment = paste0('<div><label>Assessment</label>',initSingleHint$assessment,'</div>')
    tpproduct = paste0('<div><label>Product</label>',initSingleHint$product.group.name,'</div>')
    tptype = paste0('<div><label>Intervention type</label>',initSingleHint$intervention.type,'</div>')
    tpofficial = paste0('<div><label>URL official</label>',initSingleHint$official,'</div>')
    tpnews = paste0('<div><label>URL news</label>',initSingleHint$news,'</div>')
    tpdescription = paste0('<div><label>Description</label>',initSingleHint$hint.description,'</div>')
    
    tpcontent = gsub("'","\"",paste0('<div id="top-tooltip_',initSingleHint$hint.id,'" class="tipped-content"><div class="tipped-grid"">',tpdate,tpactingAgency,tpimplementer,tpassessment,tptype,tpproduct,'</div><div class="tipped-description">',tpdescription,'</div><div class="tipped-url">',tpofficial,tpnews,'</div></div>'))
    
    initSingleHint$hint.title <- paste(initSingleHint$hint.id, initSingleHint$hint.title, sep=" - ")
    
    initSingleHint$url <- ifelse(is.na(initSingleHint$official), initSingleHint$news, initSingleHint$official)
    initSingleHint$tpcontent <- tpcontent
    
    initialHints <- generate_initial_hints(initSingleHint)
    
    initialHints <- gsub("[\r\n]", "", initialHints)
    
    reassign <- paste0("$('",initialHints,"').hide().appendTo('#hintContainer').fadeIn(300);")
    
    # If hint state == 2 or 8 and it is not an intervention -> add to collection
    if (moveHint$hint.state.id %in% c(2,8) & moveHint$is.intervention == 0) {
      runjs(reassign)
      hint.container$hint.ids <- c(hint.container$hint.ids, moveHint$hint.id)
      
      
      # if hint state != 2 or 8, or the hint is an intervention
    } else {
      
      # if hint is NOT a gta intervention -> get hint values
      if (moveHint$is.intervention == 0) {
        
        initialJurisdictions <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT jurisdiction_name FROM gta_jurisdiction_list WHERE jurisdiction_id IN (SELECT jurisdiction_id FROM bt_hint_jurisdiction WHERE hint_id = ",moveHint$hint.id,");"))))
        initialProduct <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT product_group_name FROM b221_product_group_list WHERE product_group_id IN (SELECT product_group_id FROM b221_hint_product_group WHERE hint_id = ",moveHint$hint.id,");"))))
        initialType <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT intervention_type_name FROM b221_intervention_type_list WHERE intervention_type_id IN (SELECT apparent_intervention_id FROM b221_hint_intervention WHERE hint_id = ",moveHint$hint.id,");"))))
        initialAssessment <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT assessment_name FROM b221_assessment_list WHERE assessment_id IN (SELECT assessment_id FROM b221_hint_assessment WHERE hint_id = ",moveHint$hint.id,");"))))
        
        initialJurisdictions <- unlist(strsplit(na.omit(as.character(initialJurisdictions)), " ; "))
        initialType <- unlist(strsplit(na.omit(as.character(initialType)), " ; "))
        initialProduct <- unlist(strsplit(na.omit(as.character(initialProduct)), " ; "))
        initialAssessment <- unlist(strsplit(na.omit(as.character(initialAssessment)), " ; "))
        
        initialJurisdictions <- ifelse(is.null(initialJurisdictions),character(0),initialJurisdictions)
        initialType <- ifelse(is.null(initialType),character(0),initialType)
        initialProduct <- ifelse(is.null(initialProduct),character(0),initialProduct)
        initialAssessment <- ifelse(is.null(initialAssessment),character(0),initialAssessment)
        
        # if collection attributes are locked -> compare hint values with current collection values
        if (lockHint()) {
          
          colImplementer <- input$initImplementer
          colType <- input$initType
          colProduct <- input$initProduct
          colAssessment <- input$initAssessment
          
          
          if (length(c(
            setdiff(union(colImplementer, initialJurisdictions),intersect(colImplementer, initialJurisdictions)),
            setdiff(union(colType, initialType),intersect(colType, initialType)),
            setdiff(union(colAssessment, initialAssessment),intersect(colAssessment, initialAssessment)),
            setdiff(union(colProduct,initialProduct),intersect(colProduct,initialProduct))
            
            # if freelancer adds hint with different values, display error message
          )) > 0 & prm$freelancer == 1) {
            showNotification("This hint cannot be added, as it stands in conflict with an included hint.", duration = 3)
          } else {
            runjs(reassign)
            hint.container$hint.ids <- c(hint.container$hint.ids, moveHint$hint.id)
          }
        } else {
          runjs(reassign)
          hint.container$hint.ids <- c(hint.container$hint.ids, moveHint$hint.id)
          
          updateSelectInput(session = session, inputId = "initImplementer", selected = initialJurisdictions)
          updateSelectInput(session = session, inputId = "initType", selected = initialType)
          updateSelectInput(session = session, inputId = "initProduct", selected = initialProduct)
          updateSelectInput(session = session, inputId = "initAssessment", selected = initialAssessment)
          
          if (prm$freelancer == 1) {
            runjs(paste0("$('.initialValues').addClass('locked')"))
            lockHint <- lockHint(TRUE)
          }
        }
        
        # If selected Hint is a gta intervention
      } else {
        
        colImplementerId = as.numeric(mapvalues(input$initImplementer, country.list$jurisdiction.name, country.list$jurisdiction.id, warn_missing = F))
        colTypeId = as.numeric(mapvalues(input$initType, type.list$intervention.type.name, type.list$intervention.type.id, warn_missing = F))
        colProductId = as.numeric(mapvalues(input$initProduct, product.list$product.group.name, product.list$product.group.id, warn_missing = F))
        colAssessmentId = as.numeric(mapvalues(input$initAssessment, assessment.list$assessment.name, assessment.list$assessment.id, warn_missing = F))
        
        # Get new attributes for collection
        attributes = bt_find_collection_attributes(new.collection.name = input$newCollection, hints.id = c(hint.container$hint.ids, moveHint$hint.id), starred.hint.id = hint.container$starred, country = colImplementerId, product = colProductId, intervention = colTypeId, assessment = colAssessmentId, relevance = 1, announcement.date = input$initAnnouncement, implementation.date = input$initImplementation, removal.date = input$initRemoval)
        
        colImplementerId = mapvalues(attributes$country, country.list$jurisdiction.id, country.list$jurisdiction.name, warn_missing = F)
        colTypeId = mapvalues(attributes$intervention, type.list$intervention.type.id, type.list$intervention.type.name, warn_missing = F)
        colProductId = mapvalues(attributes$product, product.list$product.group.id, product.list$product.group.name, warn_missing = F)
        colAssessmentId = mapvalues(attributes$assessment, assessment.list$assessment.id, assessment.list$assessment.name, warn_missing = F)
        
        updateSelectInput(session = session, inputId = "initImplementer", selected = colImplementerId)
        updateSelectInput(session = session, inputId = "initType", selected = colTypeId)
        updateSelectInput(session = session, inputId = "initProduct", selected = colProductId)
        updateSelectInput(session = session, inputId = "initAssessment", selected = colAssessmentId)
        
        runjs(reassign)
        hint.container$hint.ids <- c(hint.container$hint.ids, moveHint$hint.id)
        
        runjs(paste0("$('.initialValues').addClass('locked')"))
        lockHint <- lockHint(TRUE)
      }
    }
    
    runjs("$('.tooltip-create-top').tooltipster({
                                theme: 'tooltipster-noir',
                                contentCloning: true,
                                maxWidth: 600,
                                arrow:false,
                                animationDuration: 150,
                                trigger: 'hover',
                                triggerOpen: {
                                    mouseenter: true
                                },
                                triggerClose: {
                                    click: true,
                                    scroll: true
                                }
                              })")
  })
  
  
  
  
  
  # Collection UI: Rename Collections ---------------------------------------
  
  observeEvent(input$renameCollection, {
    print('SUCCESS SAVE')
    print(input$renameCollection)
    stats <- jsonlite::fromJSON(input$renameCollection)
    
    collectionId <- as.numeric(stats$id)
    collectionName <- stats$name
    gta_sql_multiple_queries(paste0("UPDATE b221_collection_log SET collection_name = '",collectionName,"' WHERE collection_id = ",collectionId,";"), output.queries = 1)
  })
  
  
  # Collection UI: Select Collection ----------------------------------------
  
  # SELECT ROWS MECHANISM COLLECTION TABLE
  observeEvent(input$collectionClick, { 
    collectionId = as.numeric(input$collectionClick)
    collectionName <- gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT collection_name FROM b221_collection_log WHERE collection_id = ",collectionId,";")))
    collectionHints <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT hint_id, hint_title FROM bt_hint_text WHERE hint_id IN (SELECT hint_id FROM b221_hint_collection WHERE collection_id = ",collectionId,");"))))
    
    initialHints <- get_info_by_collection_id(collection.id = collectionId)
    
    hint.container$hint.ids <- c(unique(initialHints$hint.id))
    
    # check if gta intervention amongst hints
    gtaHint <- FALSE
    
    if (nrow(initialHints)>0) {
      initialHints$hint.title <- paste(initialHints$hint.id, initialHints$hint.title, sep=" - ")
      
      initialHints$tpcontent = paste0('<div id="top-tooltip_',initialHints$hint.id,'" class="tipped-content"><div class="tipped-grid"">',
                                      '<div><label>Date</label>',initialHints$hint.date,'</div>',
                                      '<div><label>Acting Agency</label>',initialHints$acting.agency,'</div>',
                                      '<div><label>Implementer</label>',initialHints$jurisdiction.name,'</div>',
                                      '<div><label>Assessment</label>',initialHints$assessment,'</div>',
                                      '<div><label>Intervention type</label>',initialHints$intervention.type,'</div>',
                                      '<div><label>Product</label>',initialHints$product.group.name,'</div>',
                                      '</div><div class="tipped-description">',
                                      '<div><label>Description</label>',initialHints$hint.description,'</div>',
                                      '</div><div class="tipped-url">',
                                      '<div><label>URL official</label>',initialHints$official,'</div>',
                                      '<div><label>URL news</label>',initialHints$news,'</div>',
                                      '</div></div>')
      
      initialHints$url <- ifelse(is.na(initialHints$official), initialHints$news, initialHints$official)
      
      if (any(initialHints$is.intervention == 1)) {
        gtaHint <- TRUE
      }
      
      initialHints = gsub("'","\"",generate_initial_hints(initialHints))
      
    } else {
      initialHints <- c()
    }
    
    updateTextInput(session = session, inputId = "newCollection", value = collectionName)
    
    query = paste0("SELECT cltn_log.collection_id, cltn_log.collection_name, 
                        GROUP_CONCAT(DISTINCT(jur_list.jurisdiction_name) SEPARATOR ' ; ') AS jurisdiction_name,
                        GROUP_CONCAT(DISTINCT(ass_list.assessment_name) SEPARATOR ' ; ') AS assessment_name,
                        GROUP_CONCAT(DISTINCT(int_list.intervention_type_name) SEPARATOR ' ; ') AS intervention_type_name,
                        GROUP_CONCAT(DISTINCT(prod_grp_list.product_group_name) SEPARATOR ' ; ') AS product_group_name,
                        cltn_rel.relevance, cltn_star.hint_id AS starred_hint,
                        MAX(IF(bt_date_type_list.date_type_name='announcement', col_date.date, NULL )) AS announcement_date,
                          MAX(IF(bt_date_type_list.date_type_name='implementation', col_date.date, NULL )) AS implementation_date,
                          MAX(IF(bt_date_type_list.date_type_name='removal', col_date.date, NULL )) AS removal_date
                        FROM (SELECT * FROM b221_collection_log WHERE collection_id = ",collectionId,") cltn_log
                        LEFT JOIN b221_collection_jurisdiction cltn_jur ON cltn_jur.collection_id = cltn_log.collection_id LEFT JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_id = cltn_jur.jurisdiction_id
                        LEFT JOIN b221_collection_star cltn_star ON cltn_star.collection_id = cltn_log.collection_id
                        LEFT JOIN b221_collection_assessment cltn_ass ON cltn_ass.collection_id = cltn_log.collection_id LEFT JOIN b221_assessment_list ass_list ON cltn_ass.assessment_id = ass_list.assessment_id
                        LEFT JOIN b221_collection_intervention cltn_int ON cltn_int.collection_id = cltn_log.collection_id LEFT JOIN b221_intervention_type_list int_list ON int_list.intervention_type_id = cltn_int.intervention_type_id
                        LEFT JOIN b221_collection_product_group cltn_prod ON cltn_prod.collection_id = cltn_log.collection_id LEFT JOIN b221_product_group_list prod_grp_list ON prod_grp_list.product_group_id = cltn_prod.product_group_id
                        LEFT JOIN b221_collection_relevance cltn_rel ON cltn_rel.collection_id = cltn_log.collection_id
                        LEFT JOIN b221_collection_date col_date ON col_date.collection_id = cltn_log.collection_id LEFT JOIN bt_date_type_list ON col_date.date_type_id = bt_date_type_list.date_type_id
                        GROUP BY cltn_log.collection_id;")
    
    collectionStats <- gta_sql_get_value(query)
    
    updateSelectInput(session = session, inputId = "initImplementer", selected = unlist(strsplit(collectionStats$jurisdiction.name, " ; ")))
    updateSelectInput(session = session, inputId = "initType", selected = unlist(strsplit(collectionStats$intervention.type.name, " ; ")))
    updateSelectInput(session = session, inputId = "initProduct", selected = unlist(strsplit(collectionStats$product.group.name, " ; ")))
    updateSelectInput(session = session, inputId = "initAssessment", selected = unlist(strsplit(collectionStats$assessment.name, " ; ")))
    updateDateInput(session = session, inputId = "initAnnouncement", value = unlist(strsplit(collectionStats$announcement.date, " ; ")))
    updateDateInput(session = session, inputId = "initImplementation", value = unlist(strsplit(collectionStats$implementation.date, " ; ")))
    updateDateInput(session = session, inputId = "initRemoval", value = unlist(strsplit(collectionStats$removal.date, " ; ")))
    
    # if gta intervention in collection: Lock collection values
    if (gtaHint){
      lockHint <- lockHint(TRUE)
      runjs(paste0("$('.initialValues').addClass('locked')"))
    } else {
      lockHint <- lockHint(FALSE)
      runjs(paste0("$('.initialValues').removeClass('locked')"))
    }
    
    
    runjs("$('#hintContainer .added').fadeOut(300, function(){$(this).remove();});")
    runjs(paste0("$('#b221-slideInRight .collectionHeader')[0].id = 'existingCollection_",collectionId,"';console.log('changed id');"))
    if (length(initialHints)>0) {
      initialHints <- gsub("[\r\n]", "", initialHints)
      runjs(paste0("$('",paste0(initialHints, collapse=""),"').hide().appendTo('#hintContainer').fadeIn(300);"))
      runjs("$('.tooltip-create-top').tooltipster({
                                  theme: 'tooltipster-noir',
                                  contentCloning: true,
                                  maxWidth: 600,
                                  arrow:false,
                                  animationDuration: 150,
                                  trigger: 'hover',
                                  triggerOpen: {
                                      mouseenter: true
                                  },
                                  triggerClose: {
                                      click: true,
                                      scroll: true
                                  }
                                })")
      
      if(nrow(collectionStats)>0) {
        if (is.na(collectionStats$starred.hint)==F) {
          runjs(paste0("$('#hintContainer #hintId_",collectionStats$starred.hint,"').addClass('starred');"))
        }
      }
    }
  })
  
  # COLLECT ALL CLICKS ON LEADS-ITEM
  observeEvent(input$submit, {
    # collect data => pass it to collectedData input
    shinyjs::runjs("collectData();")
  })
  
  # VALIDATE DATA, PASS IT TO DATABASE AND SHOW IT TO USER
  observeEvent(input$collectedData, {
    print("COLLECTDATA")
    changes <- jsonlite::fromJSON(input$collectedData)
    
    changes$comment = ifelse(is.na(changes$comment), NA_character_, changes$comment)
    changes$country = ifelse(is.na(changes$country), list(NA_character_), strsplit(as.character(changes$country), split=' ; '))
    changes$product = ifelse(is.na(changes$product), list(NA_character_), strsplit(as.character(changes$product), split=' ; '))
    changes$intervention = ifelse(is.na(changes$intervention), list(NA_character_), strsplit(as.character(changes$intervention), split=' ; '))
    
    print(changes)
    
    print(paste0("Is freelancer: ",ifelse(prm$freelancer == 1,1,0)))
    
    b221_process_display_info(is.freelancer = ifelse(prm$freelancer == 1,1,0) ,user.id = user$id, processed.rows = changes) # freelancer editor is reversed
    
    # SQL INSERT HERE: ONLY MARKED AS RELEVANT VALUES ARE INSERTED HERE
    # THIS DATA NEEDS TO BE CHECKED AGAINST EXISTING CLASSIFICATIONS AND ONLY UPDATED IF THE VALUES HAVE CHANGED:
    # data comes in the form data.frame(id = 33227, 
    #                                    clicked = 1, 
    #                                    country = "China ; Switzerland",  
    #                                    product = "medicines", 
    #                                    intervention = "import barrier", 
    #                                    assessment = "unclear", 
    #                                    url = "www.url.com",
    #                                    official = 1,
    #                                    comment = 'asd')
    
  })
  
  observeEvent(input$showError, {
    if(input$showError == "allFields"){
      showNotification("Please fill out all necessary values", duration = 3)
    }
  })
  
  observe({
    
    click('loadMoreLeads')
    
  })
  
}