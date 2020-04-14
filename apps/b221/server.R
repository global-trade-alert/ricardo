# Define reactive values used in this app module up here, these will be passed automatically
# Don't forget to add these variables as function parameters as well

# SERVER
b221server <- function(input, output, session, user, app, prm, ...) {

  ns <- NS("b221")
  
  observe({
    # print(prm)
  })
  
  # CREATE DATAFRAMES FOR SELECTION FIELDS
  country.list <- gta_sql_get_value(sqlInterpolate(pool, "SELECT DISTINCT jurisdiction_name FROM gta_jurisdiction_list;"))
  assessment.list <- gta_sql_get_value(sqlInterpolate(pool, "SELECT DISTINCT assessment_name FROM b221_assessment_list;"))
  product.list <- gta_sql_get_value(sqlInterpolate(pool, "SELECT DISTINCT product_group_name FROM b221_product_group_list;"))
  type.list <- gta_sql_get_value(sqlInterpolate(pool, "SELECT DISTINCT intervention_type_name FROM b221_intervention_type_list;"))
  
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
      columnDefs = list(list(visible = FALSE, targets = c(0,1,2,3,4,5,6,7,8,9)), list(sortable=FALSE, targets = c(0))),
      language = list(
      zeroRecords = "No more leads available."),
      rowCallback = JS("function ( row, data ) {
                            if (data[3]==null) {
                              var description = 'No description available';
                            } else {
                              var description = data[3];
                            }
                            if (description.length > 300) {
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
                            
                            let country = data[6];
                            let product = data[7];
                            let intervention = data[8];
                            let submit = '<div class=\\'submission\\'><button id=\\'submit_'+data[5]+'\\' type=\"button\" class=\"btn btn-default action-button\">Save changes</button></div>';
                            let official = '<div class=\\'is-official\\'><label for=\"official\">URL official</label><div class=\\'checkbox\\'><input type=\"checkbox\" id=\\'official_'+data[5]+'\\' name=\"official\" value=\"non-official\"></div></div>';
                            let assessment = data[9];
                            
                            
                            var actingAgency = '<div class=\\'acting-agency\\'><label>Acting Agency</label><div class=\\'value\\'>'+data[1]+'</div></div>';
                            var title = '<div class=\\'title-row\\'><div class=\\'act-title\\'>'+data[2]+'</div></div>';
                            var descr = '<div class=\\'middle-row\\'><div class=\\'act-description\\'>'+description+'</div><div class=\\'gradient-bottom\\'><div class=\\'gradient-inner\\'></div></div></div>';
                            var buttons = '<div class=\\'bottom-row no-touch\\'><div class=\\'show-more no-touch\\'><img src=\\''+image+'\\' class=\\'svg no-touch\\'></div>'+urlimage+'<div class=\\'collection-add no-touch\\'><img src=\\'www/b221/collection.svg\\' class=\\'svg no-touch\\'></div></div>';
                            var options = '<div class=\\'top-row\\'>'+country+product+actingAgency+intervention+assessment+official+'</div>';
                            var middle = '<div class=\\'middle-col'+readMore+'\\'>'+descr+'</div>';
                            var right = '<div class=\\'right-col\\'><div id=\\'discard\\' class=\\'evaluate\\'><span class=\\'material-icons\\'>cancel</span></div><div id=\\'relevant\\' class=\\'evaluate\\'><span class=\\'material-icons\\'>check_circle</span></div></div>';
                           $(row)
                           .append('<div id=\\'leadsID_'+data[5]+'\\' class=\\'leads-item\\'><div class=\\'left\\'>'+title+options+middle+buttons+submit+'</div><div class=\\'right\\'>'+right+'</div>')
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
      }); basicUI(); submitSingleHint();",if(prm$autosubmit==1){"callLeadsDismiss(); checkLeads();"} else {"checkLeadsManual();"})),
    extensions = "Select",
    selection = "none"
    ),
    server = T)

  
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
      pull.group = ifelse(user$group <= 2, "B221 - freelancer desk", "B221 - editor desk")
      pull.type=prm$implementer
      pull.start = prm$datestart
      pull.end = prm$dateend
      pull.agency = prm$actingagency
      
      # KS: use bt_attribute_hint_processing(user.id = user.id, hint.state = NULL) where hint.state is for example 'B221 - freelancer desk'
      # bt_attribute_hint_processing(user.id = user$id, hint.state = "B221 - freelancer desk")
      # Then run to retrieve a dataframe to display: 
      # b221_pull_display_info(user.id = user$id, is.freelancer = T)
      
      leads.output <- gta_sql_get_value(sqlInterpolate(pool, "SELECT country_lead, acting_agency, act_title_en, act_description_en, act_url, lead_id FROM bt_leads_core WHERE (bin_check = false AND relevant = false) ORDER BY relevance_probability DESC LIMIT 20;"))
      
      # Adding option fields for output (selected = x[] defines the value currently selected and will be defined by the data pull function)
      leads.output$select_country = apply(leads.output,1, function(x){
        as.character(selectizeInput(paste0('country_',x['lead.id']),
                                    selected = x['country.lead'],
                                    label = 'Implementing country',
                                    choices = country.list,
                                    multiple = TRUE)
        )
      })
      leads.output$select_product = apply(leads.output,1, function(x){
        as.character(selectizeInput(paste0('product_',x['lead.id']),
                                    label = 'Product',
                                    selected = "",
                                    choices = product.list,
                                    multiple = TRUE)
        )
      })
      leads.output$select_intervention = apply(leads.output,1, function(x){
        as.character(selectizeInput(paste0('intervention_',x['lead.id']),
                                    label = 'Intervention Type',
                                    selected = "",
                                    choices = type.list,
                                    multiple = TRUE)
        )
      })
      leads.output$select_assessment = apply(leads.output,1, function(x){
        as.character(selectizeInput(paste0('assessment_',x['lead.id']),
                                    label = 'Assessment',
                                    selected = "",
                                    choices = assessment.list,
                                    multiple = FALSE)
        )
      })
      
      leads.output <<- leads.output
    })
    
    
    # OBSERVE SHINY JS CHECK LEADS EVENT FOR ITEMS PASSING SCREEN TOP
    observeEvent(input$checkLeads, {
      id <- as.numeric(gsub("leadsID_","", input$checkLeads))
      print(paste0("Scrolled: ",id))
      if (prm$autosubmit == 1){ # check current state of the app
        
        # SQL INSERT HERE: SAVE DISMISSED STATUS FOR HINT ID
        # hint.id is serve via id
        
        # OLD QUERY:
        # gta_sql_update_table(sqlInterpolate(pool, "UPDATE bt_leads_core SET bin_check = true, bin_recovered = false, relevant = false, sent_out = false, evaluation = false WHERE lead_id = ?leadsID;", leadsID = id))
        # gta_sql_update_table(sqlInterpolate(pool, "INSERT INTO bt_leads_checked VALUES (?leadsID, ?userID);", leadsID = id, userID = user$id))
      }
     })
    
    
    # OBSERVE CLICKS ON LEADS-ITEM AND CHANGE DATABSE ENTRY ACCORDINGLY
    observeEvent(input$checkLeadsClick, {
     runjs(paste0("Shiny.unbindAll();basicUI();"))
     id <- as.numeric(gsub("leadsID_","", input$checkLeadsClick[2]))
     print(paste0("Clicked: ",id))
      
     # SQL INSERT HERE: UPDATE HINT STATUS FOR DISMISSED HINT
     # hint.id is served via "id"
     
     # OLD QUERY
      # gta_sql_update_table(sqlInterpolate(pool, "UPDATE bt_leads_core SET bin_check = true, bin_recovered = false, relevant = false, sent_out = false, evaluation = false WHERE lead_id = ?leadsID;", leadsID = id))
      
    })
    
    # OBSERVE CLICKS ON COLLECTION BUTTON AND OPEN SLIDEIN
    observeEvent(input$collectionAdd, {
      removeUI(selector = ".removeslideinui",immediate = T)
      print("removingUI")
      print(as.numeric(gsub("leadsID_","", input$collectionAdd)))
      hintId <- as.numeric(gsub("leadsID_","", input$collectionAdd))
      currenthintId <<- hintId
      
      # TO DO: CHECK IF HINT IS PART OF COLLECTION ALREADY, IF YES, GET COLLECTION VALUES, IF NO, GET HINT VALUES AND ADD TO INITIALXXX VARIABLES
      initialHint <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT hint_id, hint_title 
                                                                          FROM bt_hint_text
                                                                          WHERE hint_id = ",hintId,";"))))
      print(initialHint)
      print(paste0("SELECT jurisdiction_name FROM gta_jurisdiction_list WHERE hint_id IN (SELECT jurisdiction_id FROM bt_hint_jurisdiction WHERE hint_id = ",hintId,");"))
      initialJurisdictions <- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT jurisdiction_name FROM gta_jurisdiction_list WHERE jurisdiction_id IN (SELECT jurisdiction_id FROM bt_hint_jurisdiction WHERE hint_id = ",hintId,");"))))
      print(initialJurisdictions)
      
      
      initialName = "Enter Collection Name..."
      initialProduct = ""
      initialType = ""
      initialAssessment = ""
      initialHints = paste0('<div id="hintId_',initialHint$hint.id,'" class="hint-item initial"><div class="hint-title">',initialHint$hint.title,'</div></div>')
      
      print(initialHint)
      insertUI(immediate = T, selector = "#b221-slideInRight",ui = tagList(
        tags$div(id=paste0("hintSlideIn_",hintId),
                 class="removeslideinui",
                 tags$div(class="newCollection",
                          id = "insertExistingCollection",
                          tags$div(id="collectionContainer",
                                   class="removeCollectionsContainer",
                                   tags$div(class="collectionHeader",
                                   textInput(ns("newCollection"),
                                             label=NULL,
                                             placeholder=initialName),
                                   actionButton(ns("saveCollection"),
                                                label="Save",
                                                icon = icon("save"))),
                                   tags$div(class="initialValues",
                                            selectInput(ns("initImplementer"),
                                                        label="Implementer",
                                                        choices = country.list,
                                                        selected = paste0(initialJurisdictions, collapse=", "),
                                                        multiple = T),
                                            selectInput(ns("initType"),
                                                        label="Intervention Type",
                                                        choices = type.list,
                                                        selected = paste0(initialType, collapse=", "),
                                                        multiple = T),
                                            selectInput(ns("initProduct"),
                                                        label="Product",
                                                        choices = product.list,
                                                        selected = paste0(initialProduct, collapse=", "),
                                                        multiple = T),
                                            selectInput(ns("initAssessment"),
                                                        label="Assessment",
                                                        choices = assessment.list,
                                                        selected = paste0(initialAssessment, collapse=", "),
                                                        multiple = F)),
                                   tags$h4("Hints included"),
                                   tags$div(id="hintContainer",
                                   HTML(initialHints)))),
                 tags$div(class="lower-tables",
                 tags$div(class="collectionTable",
                          tags$h3("Add to existing collection"),
                          dataTableOutput(ns("collectionTable"))),
                 tags$div(class="singleHintTable",
                          tags$h3("Add Hints to collection"),
                          dataTableOutput(ns("singleHintsTable")))
                 ),
                 tags$div(class="bottom-bar",
                          actionButton(ns("loadCollections"),
                                       NULL,
                                       class="hidden"))
                 )
             )
        )
      runjs(paste0("Shiny.bindAll();basicUI();"))
      runjs("$('#b221-slideInRight').addClass('open');")
      runjs("$('#b221-slideInRight').trigger('loadCollectionSlideIn');")
      
      })
    
    observeEvent(input$saveCollection, {
      runjs('saveNewCollection();')
    })
    
    observeEvent(input$saveNewCollection, {
      print(input$saveNewCollection)
      colData <- jsonlite::fromJSON(input$saveNewCollection)
      
      colName <- input$newCollection
      colImplementer <- input$initImplementer
      colType <- input$initType
      colProduct <- input$initProduct
      colAssessment <- input$initAssessment
      colID <- colData$collectionID
      colHints <- colData$childIds
      
      
      # SQL INSERT HERE: UPDATE OR CREATE COLLECTION, 
      # IF NEW COLLECTION: colID == "newCollection"
      # IF UPDATING COLLECTION: colID == "collectionID_XXXX"
      
      # VALUES FOR EACH HINT MUST BE UPDATED TO VALUES FROM ABOVE
      # FOR EXISTING COLLECTION, IT MUST BE CHECKED IF HINTS HAVE BEEN REMOVED OR ADDED
      
    })
    
    output$collectionTable <- DT::renderDataTable(DT::datatable(
      collections(),
      rownames = FALSE,
      escape = FALSE,
      # USE TO SELECT WHEN SEARCH FIELD IS ACTIVE
      options = list(
        pagingType = 'simple_numbers',
        pageLength = 10,
        columnDefs = list(list(visible = FALSE, targets = c(0,1)), list(sortable=TRUE, targets = c(0))),
        language = list(
          paginate = list("next"="<img src='www/b221/arrow_forward.svg'>", previous="<img src='www/b221/arrow_back.svg'>"),
          zeroRecords = "No more leads available.",
          search = "_INPUT_",
          searchPlaceholder = "Filter"),
        rowCallback = JS("function ( row, data ) {
                           $(row)
                           .append('<div id=\\'collection_'+data[0]+'\\' class=\\'collection-item\\'><div class=\\'collection-title\\'>'+data[1]+'</div></div>');
                           return row; }"),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'), # reset
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }') # bind select boxes to Shiny
      ),
      callback = JS(""),
      extensions = "Select",
      selection = "single"
    ),
    server = T)
    
    # LOAD COLLECTIONS FOR COLLECTIONS SLIDE IN
    collections <- eventReactive(input$loadCollections, {
      print("Collections refresh")
      collectionsOutput <- gta_sql_get_value(sqlInterpolate(pool, "SELECT collection_id, collection_name FROM b221_collection_log;"))
      collectionsOutput <<- collectionsOutput
    })
    
    output$singleHintsTable <- DT::renderDataTable(DT::datatable(
      singleHints(),
      rownames = FALSE,
      escape = FALSE,
      # USE TO SELECT WHEN SEARCH FIELD IS ACTIVE
      options = list(
        pagingType = 'simple_numbers',
        pageLength = 10,
        columnDefs = list(list(visible = FALSE, targets = c(0,1)), list(sortable=TRUE, targets = c(0))),
        language = list(
          paginate = list("next"="<img src='www/b221/arrow_forward.svg'>", previous="<img src='www/b221/arrow_back.svg'>"),
          zeroRecords = "No more leads available.",
          search = "_INPUT_",
          searchPlaceholder = "Filter"),
        rowCallback = JS("function ( row, data ) {
                           $(row)
                           .append('<div id=\\'collection_'+data[0]+'\\' class=\\'hint-item\\'><div class=\\'hint-title\\'>'+data[1]+'</div></div>');
                           return row; }"),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'), # reset
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }') # bind select boxes to Shiny
      ),
      
      callback = JS(""),
      extensions = "Select",
      selection = "single"
    ),
    server = T)
    
    # LOAD SINGLE HINTS FOR COLLECTIONS SLIDE IN
    singleHints <- eventReactive(input$loadSingleHints, {
      print("SingleHintRefresh refresh")
      singleHintOutput <- gta_sql_get_value(sqlInterpolate(pool, "SELECT hint_id, hint_title FROM bt_hint_text LIMIT 100;"))
        Encoding(singleHintOutput[["hint.title"]]) <- "UTF-8"
      singleHintOutput <<- singleHintOutput
    })
    
    # SELECT ROWS MECHANISM HITNS TABLE
    observeEvent(input$singleHintsTable_rows_selected, { 
      moveHint <- singleHintOutput[input$singleHintsTable_rows_selected,c(1,2)]
      addHint <- paste0('<div id="hintId_',moveHint$hint.id,'" class="hint-item added"><div class="hint-title">',moveHint$hint.title,'</div><div class="remove" value="',moveHint$hint.id,'"><img src="www/b221/cancel.svg"></div></div>')
      
      runjs(paste0("$('",addHint,"').hide().appendTo('#hintContainer').fadeIn(300);"))
      runjs("removeHint();")
    })
    
    # SELECT ROWS MECHANISM COLLECTION TABLE
    observeEvent(input$collectionTable_rows_selected, { 
      chooseCollection <- collectionsOutput[input$collectionTable_rows_selected,c(1,2)]
      collectionHints <<- unique(gta_sql_get_value(sqlInterpolate(pool, paste0("SELECT hint_id, hint_title FROM bt_hint_text WHERE hint_id IN (SELECT hint_id FROM b221_hint_collection WHERE collection_id = ",chooseCollection$collection.id,");"))))
      addHint <- ""
      for(r in 1:nrow(collectionHints)) {
        addHint <- paste0(addHint, '<div id="hintId_',collectionHints$hint.id[r],'" class="hint-item added"><div class="hint-title">',gsub("'", "\"", collectionHints$hint.title[r]),'</div><div class="remove" value="',collectionHints$hint.id[r],'"><img src="www/b221/cancel.svg"></div></div>')
      }
      
      # TO DO: GET COLLECTION VALUES (IMPLEMENTER, PRODUCT ETC.) AND UPDATE INPUT FIELDS HERE
      
      
      updateTextInput(session = session, inputId = "newCollection", value = chooseCollection$collection.name)
      
      runjs("$('#hintContainer .added').fadeOut(300, function(){$(this).remove();});")
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
      print(changes)
      temp <<-  changes
      
      # validate( # validate if user is not submitting empty data
      #   validateSubmit(session, changes, input)
      # )

      # SQL, INSERT HERE ONLY MARKED AS RELEVANT VALUES ARE INSERTED HERE
      # THIS DATA NEEDS TO BE CHECKED AGAINST EXISTING CLASSIFICATIONS AND ONLY UPDATED IF THE VALUES HAVE CHANGED:
      # data comes in the form data.frame(id = 33227, ¨
      #                                    clicked = 1, 
      #                                    country = "China, Switzerland",  
      #                                    product = "medicines", 
      #                                    intervention = "import barrier", 
      #                                    assessment = "unclear", 
      #                                    official = 1)
      # NO BULK DATA, SINGLE HINTS COMING IN ONLY
      
      runjs(paste0("$('#leadsID_",changes$id,"').removeClass('show-submission');console.log('removed.submission');"))
      
    })
    
    observeEvent(input$showError, {
      if(input$showError == "allFields"){
        showNotification("Please fill out all necessary Values", duration = 3)
      }
      })
    
    observe({
      insertUI(selector = "#loadMoreButton", ui = tagList(
        actionButton(ns("loadMoreLeads"),
                     "Refresh",
                     class="blue")
      ))
    })
    
}

