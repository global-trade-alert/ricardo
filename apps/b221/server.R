# Define reactive values used in this app module up here, these will be passed automatically
# Don't forget to add these variables as function parameters as well

# SERVER
b221server <- function(input, output, session, user, app, prm, ...) {

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
      columnDefs = list(list(visible = FALSE, targets = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)), list(sortable=FALSE, targets = c(0))),
      language = list(
        zeroRecords = "No more leads available."),
      rowCallback = JS("function ( row, data ) {
                            if (data[6]==null) {
                              var collection = '<div class=\\'noPartOfCollection collection-add no-touch\\'><img src=\\'www/b221/collection.svg\\' class=\\'svg no-touch\\'></div>'
                            } else {
                              var collection = '<div id=\\'collection_'+data[6]+'\\' class=\\'partOfCollection collection-add no-touch\\'><img src=\\'www/b221/collection-added.svg\\' class=\\'svg no-touch\\'></div>'
                            }
                            if (data[3]==null) {
                              var description = 'No description available';
                            } else {
                              var description = data[3];
                            }
                            if (description.length > 450) {
                                var readMore = ' readMore';
                                var image = 'www/b221/expand.svg';
                            } else {
                                var readMore = '';
                                var image = 'www/b221/expandgrey.svg';
                            }
                            if (data[4]==null) {
                              var urlimage = '<div class=\\'background-url no-touch\\'><img src=\\'www/b221/urlgrey.svg\\' class=\\'svg no-touch\\'></div>';
                            } else {
                              var url = data[4];
                              var urlimage = '<div class=\\'background-url no-touch\\'><a class=\\'no-touch\\' href=\\''+url+'\\' target=\\'_blank\\'><img src=\\'www/b221/url.svg\\' class=\\'svg no-touch\\'></a></div>';
                            }
                            
                            let country = data[13];
                            let product = data[14];
                            let intervention = data[15];
                            let submit = '<div class=\\'submission\\'><button id=\\'submit_'+data[5]+'\\' type=\"button\" class=\"btn btn-default action-button\">Save changes</button></div>';
                            let checkboxCheck = data[12] == 1 ? ' checked' : '';
                            let official = '<div class=\\'is-official\\'><label for=\"official\">URL official</label><div class=\\'checkbox\\'><input type=\"checkbox\" id=\\'official_'+data[5]+'\\' name=\"official\" value=\"non-official\"'+checkboxCheck+'></div></div>';
                            let assessment = data[16];
                            let comment = data[17];
                            
                            
                            var actingAgency = '<div class=\\'acting-agency\\'><label>Acting Agency</label><div class=\\'value\\'>'+data[1]+'</div></div>';
                            var title = '<div class=\\'title-row\\'><div class=\\'act-title\\'>'+data[2]+'</div></div>';
                            var descr = '<div class=\\'middle-row\\'><div class=\\'act-description\\'>'+description+'</div><div class=\\'gradient-bottom\\'><div class=\\'gradient-inner\\'></div></div><div class=\\'show-more no-touch\\'><img src=\\''+image+'\\' class=\\'svg no-touch\\'></div></div>';
                            var buttons = '<div class=\\'bottom-row no-touch\\'>'+urlimage+collection+'</div>';
                            var options = '<div class=\\'top-row\\'>'+country+product+actingAgency+intervention+assessment+official+'</div><div class=\\'comment\\'>'+comment+'</div>';
                            var middle = '<div class=\\'middle-col'+readMore+'\\'>'+descr+'</div>';
                            var right = '<div class=\\'right-col\\'><div id=\\'discard\\' class=\\'evaluate\\'><span class=\\'material-icons\\'>cancel</span></div><div id=\\'relevant\\' class=\\'evaluate\\'><span class=\\'material-icons\\'>check_circle</span></div></div>';
                           $(row)
                           .append('<div id=\\'leadsID_'+data[5]+'\\' class=\\'leads-item\\'><div class=\\'left\\'>'+title+middle+options+buttons+submit+'</div><div class=\\'right\\'>'+right+'</div>')
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
        leads.output <- pre.sorted.table[match(processing.hints,pre.sorted.table$hint.id),]
        rm('pre.sorted.table')
      }
      
      print(length(leads.output))
      
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
      leads.output <- leads.output
      test <<- head(leads.output)
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
      print(collectionId)
      
      if (collection) {
        query = paste0("SELECT cltn_log.collection_id, cltn_log.collection_name, 
                        GROUP_CONCAT(DISTINCT(jur_list.jurisdiction_name) SEPARATOR ' ; ') AS jurisdiction_name,
                        GROUP_CONCAT(DISTINCT(ass_list.assessment_name) SEPARATOR ' ; ') AS assessment_name,
                        GROUP_CONCAT(DISTINCT(int_list.intervention_type_name) SEPARATOR ' ; ') AS intervention_type_name,
                        GROUP_CONCAT(DISTINCT(prod_grp_list.product_group_name) SEPARATOR ' ; ') AS product_group_name,
                        cltn_rel.relevance
                        FROM b221_collection_log cltn_log
                        JOIN b221_collection_jurisdiction cltn_jur ON cltn_jur.collection_id = cltn_log.collection_id AND cltn_log.collection_id = ",collectionId," JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_id = cltn_jur.jurisdiction_id
                        JOIN b221_collection_assessment cltn_ass ON cltn_ass.collection_id = cltn_log.collection_id JOIN b221_assessment_list ass_list ON cltn_ass.assessment_id = ass_list.assessment_id
                        JOIN b221_collection_intervention cltn_int ON cltn_int.collection_id = cltn_log.collection_id JOIN b221_intervention_type_list int_list ON int_list.intervention_type_id = cltn_int.intervention_type_id
                        JOIN b221_collection_product_group cltn_prod ON cltn_prod.collection_id = cltn_log.collection_id JOIN b221_product_group_list prod_grp_list ON prod_grp_list.product_group_id = prod_grp_list.product_group_id
                        JOIN b221_collection_relevance cltn_rel ON cltn_rel.collection_id = cltn_log.collection_id
                        GROUP BY cltn_log.collection_id;")
        collectionStats <- gta_sql_get_value(query)
        
        # SQL INSERT HERE: CHECK IF HINT IS PART OF COLLECTION ALREADY, IF YES, GET COLLECTION VALUES, IF NO, GET HINT VALUES AND ADD TO initialXYZ VARIABLES
        
        initialName = collectionStats$collection.name
        initialPlaceholder = NULL
        initialProduct = unlist(na.omit(strsplit(collectionStats$product.group.name, " ; ")))
        initialType = unlist(na.omit(strsplit(collectionStats$intervention.type.name, " ; ")))
        initialAssessment = unlist(na.omit(strsplit(collectionStats$assessment.name, " ; ")))
        initialJurisdictions = unlist(na.omit(strsplit(collectionStats$jurisdiction.name, " ; ")))
        
        print(initialName)
        print(initialProduct)
        print(initialType)
        print(initialAssessment)
        print(initialJurisdictions)
        
        initialHints <- unique(gta_sql_get_value(paste0("SELECT ht_text.hint_title, ht_text.hint_id FROM b221_hint_collection col_log JOIN bt_hint_text ht_text ON ht_text.hint_id = col_log.hint_id WHERE ht_text.language_id = 1 AND col_log.collection_id = ",collectionId)))
        initialHints = paste0('<div id="hintId_',initialHints$hint.id,'" class="hint-item added"><div class="hint-title">',initialHints$hint.title,'</div><div class="remove" value="',initialHints$hint.id,'"><img src="www/b221/cancel.svg"></div></div>')
        
        slideInState = paste0("existingCollection_",collectionId)
        
        maxHint <- gta_sql_get_value(paste0("SELECT DISTINCT(hint_state_id) AS max_state FROM 
                                            (SELECT * FROM b221_hint_collection WHERE b221_hint_collection.collection_id = ",collectionId,") ht_cltn 
                                            JOIN bt_hint_log ht_log ON ht_log.hint_id = ht_cltn.hint_id;"))
        
        locked <- ifelse(any(maxHint %in% c(3:7,9)), " locked","")
        
      } else {
        
        initialPlaceholder <- "Enter new Collection Name"
        initialName = NULL
        initialJurisdictions <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT jurisdiction_name FROM bt_jurisdiction_list WHERE jurisdiction_id IN (SELECT jurisdiction_id FROM bt_hint_jurisdiction WHERE hint_id = ",hintId,");"))))
        initialProduct <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT product_group_name FROM b221_product_group_list WHERE product_group_id IN (SELECT product_group_id FROM b221_hint_product_group WHERE hint_id = ",hintId,");"))))
        initialType <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT intervention_type_name FROM b221_intervention_type_list WHERE intervention_type_id IN (SELECT apparent_intervention_id FROM b221_hint_intervention WHERE hint_id = ",hintId,");"))))
        initialAssessment <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT assessment_name FROM b221_assessment_list WHERE assessment_id IN (SELECT assessment_id FROM b221_hint_assessment WHERE hint_id = ",hintId,");"))))
        
        initialHint <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT hint_id, hint_title 
                                                                          FROM bt_hint_text
                                                                          WHERE hint_id = ",hintId,";"))))
        
        initialHints = paste0('<div id="hintId_',initialHint$hint.id,'" class="hint-item initial"><div class="hint-title">',initialHint$hint.title,'</div></div>')
        
        slideInState = "newCollection"
        
        locked = ""
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
                                                        multiple = F)),
                                   tags$h4("Hints included"),
                                   tags$div(id="hintContainer",
                                   HTML(initialHints)))
        )
        )
      runjs(paste0(" slideInBasicUI(); removeHint();"))
      runjs("$('#b221-slideInRight').addClass('open');")
      runjs("$('#b221-slideInRight').trigger('loadCollectionSlideIn');console.log('2 loading collection slide in');")
      
      })
    
    observeEvent(input$saveCollection, {
      runjs('saveNewCollection();')
    })
    
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
      colHints <- as.numeric(colData$childIds)
      colState <- colData$state
      hintId <- as.numeric(gsub("collectionContainer_","",colData$hintId))
      
      colName <<- input$newCollection
      colImplementer <<- input$initImplementer
      colType <<- input$initType
      colProduct <<- input$initProduct
      colAssessment <<- input$initAssessment
      colHints <<- as.numeric(colData$childIds)
      colState <<- colData$state
      hintId <<- as.numeric(gsub("collectionContainer_","",colData$hintId))
      
      
      print(colName)
      print(colImplementer)
      print(colType)
      print(colProduct)
      print(colAssessment)
      print(colHints)
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
        colImplementerId = as.numeric(mapvalues(colImplementer, country.list$jurisdiction.name, country.list$jurisdiction.id))
        colTypeId = as.numeric(mapvalues(colType, type.list$intervention.type.name, type.list$intervention.type.id))
        colProductId = as.numeric(mapvalues(colProduct, product.list$product.group.name, product.list$product.group.id))
        colAssessmentId = as.numeric(mapvalues(colAssessment, assessment.list$assessment.name, assessment.list$assessment.id))
        
        
      # SQL INSERT HERE: UPDATE OR CREATE COLLECTION, 
      # IF NEW COLLECTION: colID == "newCollection"
      # IF UPDATING COLLECTION: colID == "collectionID_XXXX"
        
        if (colState == "newCollection") {
          
          b221_process_collections_hints(is.freelancer = ifelse(prm$freelancer == 1, T, F), user.id = user$id, new.collection.name = colName, hints.id = colHints, country = colImplementerId, product = colProductId, intervention = colTypeId, assessment = colAssessmentId, relevance = 1, collection.unchanged = F)
          
        } else {
          
          collectionId <- as.numeric(gsub("existingCollection_","", colState))
          
          # get old collections attributes to compare
          query = paste0("SELECT cltn_log.collection_id, cltn_log.collection_name, 
                        GROUP_CONCAT(DISTINCT(jur_list.jurisdiction_name) SEPARATOR ' ; ') AS jurisdiction_name,
                        GROUP_CONCAT(DISTINCT(ass_list.assessment_name) SEPARATOR ' ; ') AS assessment_name,
                        GROUP_CONCAT(DISTINCT(int_list.intervention_type_name) SEPARATOR ' ; ') AS intervention_type_name,
                        GROUP_CONCAT(DISTINCT(prod_grp_list.product_group_name) SEPARATOR ' ; ') AS product_group_name,
                        cltn_rel.relevance
                        FROM b221_collection_log cltn_log
                        JOIN b221_collection_jurisdiction cltn_jur ON cltn_jur.collection_id = cltn_log.collection_id AND cltn_log.collection_id = ",collectionId," JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_id = cltn_jur.jurisdiction_id
                        JOIN b221_collection_assessment cltn_ass ON cltn_ass.collection_id = cltn_log.collection_id JOIN b221_assessment_list ass_list ON cltn_ass.assessment_id = ass_list.assessment_id
                        JOIN b221_collection_intervention cltn_int ON cltn_int.collection_id = cltn_log.collection_id JOIN b221_intervention_type_list int_list ON int_list.intervention_type_id = cltn_int.intervention_type_id
                        JOIN b221_collection_product_group cltn_prod ON cltn_prod.collection_id = cltn_log.collection_id JOIN b221_product_group_list prod_grp_list ON prod_grp_list.product_group_id = prod_grp_list.product_group_id
                        JOIN b221_collection_relevance cltn_rel ON cltn_rel.collection_id = cltn_log.collection_id
                        GROUP BY cltn_log.collection_id;")
          collectionStats <- gta_sql_get_value(query)
          
          if (length(c(
            setdiff(colImplementer, strsplit(collectionStats$jurisdiction.name,split = " ; ")[[1]]),
            setdiff(colType, strsplit(collectionStats$intervention.type.name,split = " ; ")[[1]]),
            setdiff(colAssessment, strsplit(collectionStats$assessment.name,split = " ; ")[[1]]),
            setdiff(colProduct, strsplit(collectionStats$product.group.name,split = " ; ")[[1]])
          )) > 0 ) {
            collectionChanged = T
          } else {
            collectionChanged = F
          }
          
          b221_process_collections_hints(is.freelancer = ifelse(prm$freelancer == 1, T, F), user.id = user$id, collection.id = collectionId, hints.id = colHints, country = colImplementerId, product = colProductId, intervention = colTypeId, assessment = colAssessmentId, relevance = 1, collection.unchanged = collectionChanged)
        
        }
        
        print(paste0("THIS IS THE COUNTRY HINT ID country_",hintId))
      
        
        
      updateSelectInput(session = session, inputId = paste0("country_",hintId), selected = colImplementer)
      updateSelectInput(session = session, inputId = paste0("product_",hintId), selected = colProduct)
      updateSelectInput(session = session, inputId = paste0("intervention_",hintId), selected = colType)
      updateSelectInput(session = session, inputId = paste0("assessment_",hintId), selected = colAssessment)
      
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
        
        runjs(paste0("$('#leadsID_",hintId,"').addClass('locked');"))
        runjs(paste0("$('#leadsID_",hintId," .collection-add').removeClass('noPartOfCollection');"))
        runjs(paste0("$('#leadsID_",hintId," .collection-add').addClass('partOfCollection');"))
        runjs(paste0("$('#leadsID_",hintId," .collection-add')[0].id = 'collection_",collectionId,"';"))
        runjs(paste0("$('#leadsID_",hintId," .collection-add img')[0].src = $('#leadsID_",hintId," .collection-add img')[0].src.replace('collection.svg', 'collection-added.svg');"))
      } else {
        runjs(paste0("$('#leadsID_",hintId," .collection-add').addClass('noPartOfCollection');"))
        runjs(paste0("$('#leadsID_",hintId," .collection-add').removeClass('partOfCollection');"))
        runjs(paste0("$('#leadsID_",hintId," .collection-add img')[0].src = $('#leadsID_",hintId," .collection-add img')[0].src.replace('collection-added.svg', 'collection.svg');"))
        runjs(paste0("$('#leadsID_",hintId," .collection-add')[0].id = '';"))
        
      }
      runjs(paste0("$('#b221-slideInRight').removeClass('open');"))
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
        columnDefs = list(list(visible = FALSE, targets = c(0:9)), list(sortable=TRUE, targets = c(0))),
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
                            
                            let tags = '<div class=\\'tags\\'>'+assessment+date+jurisdiction+type+product+'</div>';

                            let tpdate = '<div><label>Date</label>'+data[3]+'</div>';
                            let tpimplementer = '<div><label>Implementer</label>'+data[7]+'</div>';
                            let tpassessment = '<div><label>Assessment</label>'+data[2]+'</div>';
                            let tpproduct = '<div><label>Product</label>'+data[8]+'</div>';
                            let tptype = '<div><label>Intervention type</label>'+data[9]+'</div>';

                            let tpcontent = '<div id=\\'coltooltip_'+data[0]+'\\' class=\\'tipped-content\\'><div class=\\'tipped-grid\\'>'+tpdate+tpimplementer+tpassessment+tptype+tpproduct+'</div></div>';
                            
                            let tipped = '<span><span class=\\'material-icons\\'>info</span></span>';
        
                           $(row)
                           .append('<div id=\\'collection_'+data[0]+'\\' class=\\'collection-item\\'><div class=\\'left\\'>'+tags+'<div class=\\'collection-title\\'>'+data[1]+'</div></div><div class=\\'right\\'><div data-tooltip-content=\\'#coltooltip_'+data[0]+'\\' class=\\'coltooltip-create info\\'>'+tipped+'</div><div class=\\'icon\\'><span class=\\'material-icons add\\'>add_circle</span></div></div></div>'+tpcontent);

                           return row; }"),
        initComplete = JS("function createTipped() {
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
                              })
                            }")),
      callback = JS(""),
      extensions = "Select",
      selection = "single"
    ),
    server = T)
    
    # LOAD COLLECTIONS FOR COLLECTIONS SLIDE IN
    collections <- eventReactive(input$loadCollections, {
      print("Collections refresh")
      
      # collectionsOutput <- gta_sql_get_value(sqlInterpolate(pool, "SELECT collection_id, collection_name FROM b221_collection_log;"))
      collectionsOutput <- gta_sql_get_value(sqlInterpolate(pool, "SELECT cltn.collection_id, cltn.collection_name, ass_list.assessment_name, MIN(bt_hint_log.hint_date) AS hint_date,
                                                                    GROUP_CONCAT(DISTINCT(int_list.intervention_type_name) SEPARATOR ' ; ') AS intervention_type_name,
                                                                    GROUP_CONCAT(DISTINCT(prod_list.product_group_name) SEPARATOR ' ; ') AS product_group_name,
                                                                    GROUP_CONCAT(DISTINCT(jur_list.jurisdiction_name) SEPARATOR ' ; ') AS jurisdiction_name
                                                                    FROM b221_collection_log cltn
                                                                    JOIN b221_collection_assessment cltn_ass ON cltn_ass.collection_id = cltn.collection_id JOIN b221_assessment_list ass_list ON cltn_ass.assessment_id = ass_list.assessment_id
                                                                    JOIN b221_collection_intervention cltn_int ON cltn_int.collection_id = cltn.collection_id JOIN b221_intervention_type_list int_list ON cltn_int.intervention_type_id = int_list.intervention_type_id
                                                                    JOIN b221_collection_product_group cltn_prod ON cltn_prod.collection_id = cltn.collection_id JOIN b221_product_group_list prod_list ON cltn_prod.product_group_id = prod_list.product_group_id
                                                                    JOIN b221_collection_jurisdiction cltn_jur ON cltn_jur.collection_id = cltn.collection_id JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_id = cltn_jur.jurisdiction_id
                                                                    JOIN b221_hint_collection ht_cltn ON ht_cltn.collection_id = cltn.collection_id JOIN bt_hint_log ON bt_hint_log.hint_id = ht_cltn.hint_id
                                                                    GROUP BY cltn.collection_id;"))
      
      collectionsOutput$intervention.type <- gsub("export subsidy","ES",collectionsOutput$intervention.type)
      collectionsOutput$intervention.type <- gsub("domestic subsidy \\(incl\\. tax cuts, rescues etc\\.)","DS",collectionsOutput$intervention.type)
      collectionsOutput$intervention.type <- gsub("import barrier","IB",collectionsOutput$intervention.type)
      collectionsOutput$intervention.type <- gsub("export barrier","EB",collectionsOutput$intervention.type)
      collectionsOutput$intervention.type <- gsub("uncertain","UN",collectionsOutput$intervention.type)
      
      collectionsOutput$tag_country = apply(collectionsOutput,1, function(x){
        as.character(paste0("<div class='grid-row'>",paste0("<div class='tag country'>",substr(strsplit(x['jurisdiction.name'],split=" ; ")[[1]],1,20),ifelse(nchar(x['jurisdiction.name'])>20,"...",""),"</div>",collapse=""),"</div>"))
      })
      collectionsOutput$tag_product = apply(collectionsOutput,1, function(x){
        as.character(paste0("<div class='grid-row'>",paste0("<div class='tag product'>",substr(strsplit(x['product.group.name'],split=" ; ")[[1]],1,20),ifelse(nchar(x['product.group.name'])>20,"...",""),"</div>",collapse=""),"</div>"))
      })
      collectionsOutput$tag_type = apply(collectionsOutput,1, function(x){
        as.character(paste0("<div class='grid-row'>",paste0("<div class='tag type'>",substr(strsplit(x['intervention.type'],split=" ; ")[[1]],1,20),ifelse(nchar(x['intervention.type'])>20,"...",""),"</div>",collapse=""),"</div>"))
      })
      
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
        columnDefs = list(list(visible = FALSE, targets = c(0:19)), list(sortable=TRUE, targets = c(0))),
        language = list(
          paginate = list("next"="<img src='www/b221/arrow_forward.svg'>", previous="<img src='www/b221/arrow_back.svg'>"),
          zeroRecords = "No more leads available.",
          search = "_INPUT_",
          searchPlaceholder = "Filter"),
        rowCallback = JS("function ( row, data ) {
                            
                            if (! [2,8].includes(data[1])) {
                            console.log('YES LOCK');
                            var lock = ' locked';
                            } else {
                            console.log('NO LOCK');
                            var lock = '';
                            } 
                            
                            let date = '<div class=\\'grid-row\\'><div class=\\'date tag\\'>'+data[3]+'</div></div>';
                            let assessment = '<div class=\\'grid-row\\'><div class=\\'assessment tag\\'>'+data[7]+'</div></div>';
                            let product = data[18];
                            let type = data[19];
                            let jurisdiction = data[17];
                            
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
                            
                           $(row)
                           .append('<div id=\\'hint_'+data[0]+'\\' class=\\'hint-item'+lock+'\\'><div class=\\'left\\'>'+tags+'<div class=\\'hint-title\\'>'+data[5]+'</div></div><div class=\\'right\\'><div data-tooltip-content=\\'#tooltip_'+data[0]+'\\' class=\\'tooltip-create info\\'>'+tipped+'</div><div class=\\'icon\\'><span class=\\'material-icons lock\\'>lock</span><span class=\\'material-icons add\\'>add_circle</span></div></div></div>'+tpcontent);
                           return row; }"),
        initComplete = JS("function createTipped() {
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
                              })
                            }")),
      callback = JS("console.log('TEST');"),
      extensions = "Select",
      selection = "single"
    ),
    server = T)
    
    # LOAD SINGLE HINTS FOR COLLECTIONS SLIDE IN
    singleHints <- eventReactive(input$loadSingleHints, {
      print("SingleHintRefresh refresh")
      singleHintOutput <- gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT * FROM
                                                                        (SELECT ht_log.hint_id, ht_log.hint_state_id, ht_log.acting_agency, ht_log.hint_date, jur_list.jurisdiction_name, ht_txt.hint_title, ht_txt.hint_description, ass_list.assessment_name, user_prio_impl.jurisdiction_id AS prio_cty, cltn_log.collection_id, cltn_log.collection_name,
                                                                        GROUP_CONCAT(DISTINCT int_list.intervention_type_name SEPARATOR ' ; ')  AS intervention_type, 
                                                                        GROUP_CONCAT(DISTINCT prod_list.product_group_name SEPARATOR ' ; ')  AS product_group_name,
                                                                        GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='official', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS official,
                                                                        GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='news', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS news,
                                                                        GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='consultancy', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS consultancy,
                                                                        GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='others', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS others
                                                                        FROM bt_hint_log ht_log 
                                                                        JOIN bt_hint_url ht_url ON ht_url.hint_id = ht_log.hint_id AND ht_log.hint_state_id BETWEEN 2 and 9 JOIN bt_url_log ON ht_url.url_id = bt_url_log.url_id JOIN bt_url_type_list ON bt_url_type_list.url_type_id = ht_url.url_type_id
                                                                        LEFT JOIN bt_hint_jurisdiction ht_jur ON ht_log.hint_id = ht_jur.hint_id LEFT JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_id = ht_jur.jurisdiction_id
                                                                        LEFT JOIN (SELECT jurisdiction_id FROM ric_user_implementers WHERE user_id = ",user$id," AND app_id = 4) user_prio_impl ON user_prio_impl.jurisdiction_id = ht_jur.jurisdiction_id
                                                                        LEFT JOIN bt_hint_text ht_txt ON ht_txt.hint_id = ht_log.hint_id AND language_id = 1
                                                                        LEFT JOIN b221_hint_assessment ht_ass ON ht_ass.hint_id = ht_log.hint_id LEFT JOIN b221_assessment_list ass_list ON ass_list.assessment_id = ht_ass.assessment_id
                                                                        LEFT JOIN b221_hint_intervention ht_int ON ht_int.hint_id = ht_log.hint_id LEFT JOIN b221_intervention_type_list int_list ON int_list.intervention_type_id = ht_int.apparent_intervention_id
                                                                        LEFT JOIN b221_hint_product_group ht_prod_grp ON ht_prod_grp.hint_id = ht_log.hint_id LEFT JOIN b221_product_group_list prod_list ON prod_list.product_group_id = ht_prod_grp.product_group_id
                                                                        LEFT JOIN b221_hint_collection ht_cltn ON ht_cltn.hint_id = ht_log.hint_id LEFT JOIN b221_collection_log cltn_log ON cltn_log.collection_id = ht_cltn.collection_id
                                                                        GROUP BY ht_log.hint_id) unsorted_hints
                                                                        ORDER BY prio_cty DESC, hint_date DESC;")))
      Encoding(singleHintOutput[["hint.title"]]) <- "UTF-8"
      Encoding(singleHintOutput[["hint.description"]]) <- "UTF-8"
      
      singleHintOutput$intervention.type <- gsub("export subsidy","Export subsidy",singleHintOutput$intervention.type)
      singleHintOutput$intervention.type <- gsub("domestic subsidy \\(incl\\. tax cuts, rescues etc\\.)","Domestic subsidy",singleHintOutput$intervention.type)
      singleHintOutput$intervention.type <- gsub("import barrier","Import barrier",singleHintOutput$intervention.type)
      singleHintOutput$intervention.type <- gsub("export barrier","Export barrier",singleHintOutput$intervention.type)
      singleHintOutput$intervention.type <- gsub("uncertain","Unclear",singleHintOutput$intervention.type)
      
      singleHintOutput$tag_country = apply(singleHintOutput,1, function(x){
        as.character(paste0("<div class='grid-row'>",paste0("<div class='tag country'>",substr(strsplit(x['jurisdiction.name'],split=" ; ")[[1]],1,20),ifelse(nchar(x['jurisdiction.name'])>20,"...",""),"</div>",collapse=""),"</div>"))
      })
      singleHintOutput$tag_product = apply(singleHintOutput,1, function(x){
        as.character(paste0("<div class='grid-row'>",paste0("<div class='tag product'>",substr(strsplit(x['product.group.name'],split=" ; ")[[1]],1,20),ifelse(nchar(x['product.group.name'])>20,"...",""),"</div>",collapse=""),"</div>"))
      })
      singleHintOutput$tag_type = apply(singleHintOutput,1, function(x){
        as.character(paste0("<div class='grid-row'>",paste0("<div class='tag type'>",substr(strsplit(x['intervention.type'],split=" ; ")[[1]],1,20),ifelse(nchar(x['intervention.type'])>20,"...",""),"</div>",collapse=""),"</div>"))
      })
      
      
      
      print(singleHintOutput)
      singleHintOutput <<- singleHintOutput
    })
    
    # SELECT ROWS MECHANISM HITNS TABLE
    observeEvent(input$singleHintsTable_rows_selected, { 
      moveHint <- singleHintOutput[input$singleHintsTable_rows_selected,]
      rowtest <<- moveHint
      print(moveHint)
      addHint <- paste0('<div id="hintId_',moveHint$hint.id,'" class="hint-item added"><div class="hint-title">',moveHint$hint.title,'</div><div class="remove" value="',moveHint$hint.id,'"><img src="www/b221/cancel.svg"></div></div>')
      reassign <- paste0("$('",addHint,"').hide().appendTo('#hintContainer').fadeIn(300);")
      if (moveHint$hint.state.id %in% c(2,8)) {
          runjs(reassign)
        } else {
            showNotification("This hint is part of a collection already. You are not allowed to reassign it.", duration = 3)
        }
      runjs("removeHint();")
    })
    
    # SELECT ROWS MECHANISM COLLECTION TABLE
    observeEvent(input$collectionTable_rows_selected, { 
      chooseCollection <- collectionsOutput[input$collectionTable_rows_selected,c(1,2)]
      collectionId = chooseCollection$collection.id
      collectionHints <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT hint_id, hint_title FROM bt_hint_text WHERE hint_id IN (SELECT hint_id FROM b221_hint_collection WHERE collection_id = ",chooseCollection$collection.id,");"))))
      addHint <- ""
      for(r in 1:nrow(collectionHints)) {
        addHint <- paste0(addHint, '<div id="hintId_',collectionHints$hint.id[r],'" class="hint-item added"><div class="hint-title">',gsub("'", "\"", collectionHints$hint.title[r]),'</div><div class="remove" value="',collectionHints$hint.id[r],'"><img src="www/b221/cancel.svg"></div></div>')
      }
      
      updateTextInput(session = session, inputId = "newCollection", value = chooseCollection$collection.name)
      
      query = paste0("SELECT cltn_log.collection_id, cltn_log.collection_name, 
                        GROUP_CONCAT(DISTINCT(jur_list.jurisdiction_name) SEPARATOR ' ; ') AS jurisdiction_name,
                        GROUP_CONCAT(DISTINCT(ass_list.assessment_name) SEPARATOR ' ; ') AS assessment_name,
                        GROUP_CONCAT(DISTINCT(int_list.intervention_type_name) SEPARATOR ' ; ') AS intervention_type_name,
                        GROUP_CONCAT(DISTINCT(prod_grp_list.product_group_name) SEPARATOR ' ; ') AS product_group_name,
                        cltn_rel.relevance
                        FROM b221_collection_log cltn_log
                        JOIN b221_collection_jurisdiction cltn_jur ON cltn_jur.collection_id = cltn_log.collection_id AND cltn_log.collection_id = ",collectionId," JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_id = cltn_jur.jurisdiction_id
                        JOIN b221_collection_assessment cltn_ass ON cltn_ass.collection_id = cltn_log.collection_id JOIN b221_assessment_list ass_list ON cltn_ass.assessment_id = ass_list.assessment_id
                        JOIN b221_collection_intervention cltn_int ON cltn_int.collection_id = cltn_log.collection_id JOIN b221_intervention_type_list int_list ON int_list.intervention_type_id = cltn_int.intervention_type_id
                        JOIN b221_collection_product_group cltn_prod ON cltn_prod.collection_id = cltn_log.collection_id JOIN b221_product_group_list prod_grp_list ON prod_grp_list.product_group_id = prod_grp_list.product_group_id
                        JOIN b221_collection_relevance cltn_rel ON cltn_rel.collection_id = cltn_log.collection_id
                        GROUP BY cltn_log.collection_id;")
      collectionStats <- gta_sql_get_value(query)
      
      updateSelectInput(session = session, inputId = "initImplementer", selected = unlist(strsplit(collectionStats$jurisdiction.name, " ; ")))
      updateSelectInput(session = session, inputId = "initType", selected = unlist(strsplit(collectionStats$intervention.type.name, " ; ")))
      updateSelectInput(session = session, inputId = "initProduct", selected = unlist(strsplit(collectionStats$product.group.name, " ; ")))
      updateSelectInput(session = session, inputId = "initAssessment", selected = unlist(strsplit(collectionStats$assessment.name, " ; ")))
      
      runjs("$('#hintContainer .added').fadeOut(300, function(){$(this).remove();});")
      runjs(paste0("$('#b221-slideInRight .collectionHeader')[0].id = 'existingCollection_",collectionId,"';console.log('changed id');"))
      runjs(paste0("$('",addHint,"').hide().appendTo('#hintContainer').fadeIn(300);"))
      runjs("removeHint();")
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
