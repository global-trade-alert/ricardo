# Define reactive values used in this app module up here, these will be passed automatically
# Don't forget to add these variables as function parameters as well

# SERVER
b221server <- function(input, output, session, user, app, prm, ...) {

  ns <- NS("b221")
  
  observe({
    print(prm)
  })
  
  # CREATE COUNTRY LIST
  country.list <- gta_sql_get_value(sqlInterpolate(pool, "SELECT DISTINCT jurisdiction_name FROM gta_jurisdiction_list;"))
  
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
      pageLength = 10000,
      columnDefs = list(list(visible = FALSE, targets = c(0,1,2,3,4,5)), list(sortable=FALSE, targets = c(0))),
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
                            
                            var left = '<div class=\\'left-col\\'></div>';
                            var middleTop = '<div class=\\'top-row\\'><div class=\\'country-lead\\'>'+data[0]+'</div><div class=\\'acting-agency\\'>'+data[1]+'</div></div>';
                            var middleMiddle = '<div class=\\'middle-row\\'><div class=\\'act-title\\'>'+data[2]+'</div><div class=\\'act-description\\'>'+description+'</div><div class=\\'gradient-bottom\\'><div class=\\'gradient-inner\\'></div></div></div>';
                            var middleBottom = '<div class=\\'bottom-row no-touch\\'><div class=\\'show-more no-touch\\'><img src=\\''+image+'\\' class=\\'svg no-touch\\'></div>'+urlimage+'<div class=\\'collection-add no-touch\\'><img src=\\'www/b221/collection.svg\\' class=\\'svg no-touch\\'></div></div>';
                            var middle = '<div class=\\'middle-col'+readMore+'\\'>'+middleTop+middleMiddle+middleBottom+'</div>';
                            var right = '<div class=\\'right-col\\'></div>';
                           $(row)
                           .append('<div id=\\'leadsID_'+data[5]+'\\' class=\\'leads-item\\'>'+middle+right)
                           .append('</div>');
                           return row; }"),
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'), # reset
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }') # bind select boxes to Shiny
    ),
    callback = JS("table.on('click.dt','tr', function() {
                var data=table.row(this).data();
                console.log(data)
      }); checkLeads(); basicUI();"),
    extensions = "Select",
    selection = "none"
    ),
    server = T)

  
  # TABLE OUTPUT FOR PREDEFINED LIST OF WORDS
  names <- eventReactive(input$loadMoreLeads, {
      print("LEADS REFRESH")
      shinyjs::runjs("window.scrollTo(0, 0);")
      if (prm$autosubmit == 0) {
        print("submittbutton")
        removeUI(selector = "#b221-submit")
        insertUI(selector = "#submitButton", ui = tagList(
          actionButton(ns("submit"), "Submit", class="green")
        ))
      }
      # KS: use bt_attribute_hint_processing(user.id = user.id, hint.state = NULL) where hint.state is for example 'B221 - freelancer desk'
      # Then run to retrieve a dataframe to display: 
      # b221_pull_display_info(user.id = user.id, is.freelancer = F/T)
      leads.output <- gta_sql_get_value(sqlInterpolate(pool, "SELECT country_lead, acting_agency, act_title_en, act_description_en, act_url, lead_id FROM bt_leads_core WHERE (bin_check = false AND relevant = false) ORDER BY relevance_probability DESC LIMIT 100;"))
      leads.output <<- leads.output
    })
    
    
    # OBSERVE SHINY JS CHECK LEADS EVENT FOR ITEMS PASSING SCREEN TOP
    observeEvent(input$checkLeads, {
      id <- as.numeric(gsub("leadsID_","", input$checkLeads))
      print(paste0("Scrolled: ",id))
      if (prm$autosubmit == 1){ # check current state of the app
        gta_sql_update_table(sqlInterpolate(pool, "UPDATE bt_leads_core SET bin_check = true, bin_recovered = false, relevant = false, sent_out = false, evaluation = false WHERE lead_id = ?leadsID;", leadsID = id))
        gta_sql_update_table(sqlInterpolate(pool, "INSERT INTO bt_leads_checked VALUES (?leadsID, ?userID);", leadsID = id, userID = user$id))
      }
     })
    
    
    # OBSERVE CLICKS ON LEADS-ITEM AND CHANGE DATABSE ENTRY ACCORDINGLY
    observeEvent(input$checkLeadsClick, {
     id <- as.numeric(gsub("leadsID_","", input$checkLeadsClick[2]))
     print(paste0("Clicked: ",id))
     if (prm$autosubmit == 1){ # check current state of the app
         if (input$checkLeadsClick[1]==T) {
           gta_sql_update_table(sqlInterpolate(pool, "UPDATE bt_leads_core SET bin_check = true, bin_recovered = true, relevant = true, sent_out = false, evaluation = true WHERE lead_id = ?leadsID;", leadsID = id))
         } else if (input$checkLeadsClick[1]==F) {
           gta_sql_update_table(sqlInterpolate(pool, "UPDATE bt_leads_core SET bin_check = true, bin_recovered = false, relevant = false, sent_out = false, evaluation = false WHERE lead_id = ?leadsID;", leadsID = id))
       }
     }
    })
    
    # OBSERVE CLICKS ON LEADS-ITEM AND OPEN POPUP
    observeEvent(input$collectionAdd, {
      removeUI(selector = ".removeslideinui")
      print(as.numeric(gsub("leadsID_","", input$collectionAdd)))
      hintId <- as.numeric(gsub("leadsID_","", input$collectionAdd))
      currenthintId <<- hintId
      
      insertUI(selector = "#slideInRight",ui = tagList(
        tags$div(id=paste0("hintSlideIn_",hintId),
                 class="removeslideinui",
                 tags$div(class="collectionTable",
                          dataTableOutput(ns("collectionTable"))),
                 tags$div(class="bottom-bar",
                          actionButton(ns("loadCollections"),
                                       NULL,
                                       class="blue"))
                 )
             )
        )
      shinyjs::runjs("$('#slideInRight').addClass('open');")
      })
    
    
    output$collectionTable <- DT::renderDataTable(DT::datatable(
      collections(),
      rownames = FALSE,
      escape = FALSE,
      # USE TO SELECT WHEN SEARCH FIELD IS ACTIVE
      options = list(
        pageLength = 10000,
        columnDefs = list(list(visible = FALSE, targets = c()), list(sortable=TRUE, targets = c(0))),
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
                            
                            var left = '<div class=\\'left-col\\'></div>';
                            var middleTop = '<div class=\\'top-row\\'><div class=\\'country-lead\\'>'+data[0]+'</div><div class=\\'acting-agency\\'>'+data[1]+'</div></div>';
                            var middleMiddle = '<div class=\\'middle-row\\'><div class=\\'act-title\\'>'+data[2]+'</div><div class=\\'act-description\\'>'+description+'</div><div class=\\'gradient-bottom\\'><div class=\\'gradient-inner\\'></div></div></div>';
                            var middleBottom = '<div class=\\'bottom-row no-touch\\'><div class=\\'show-more no-touch\\'><img src=\\''+image+'\\' class=\\'svg no-touch\\'></div>'+urlimage+'<div class=\\'collection-add no-touch\\'><img src=\\'www/b221/collection.svg\\' class=\\'svg no-touch\\'></div></div>';
                            var middle = '<div class=\\'middle-col'+readMore+'\\'>'+middleTop+middleMiddle+middleBottom+'</div>';
                            var right = '<div class=\\'right-col\\'></div>';
                           $(row)
                           .append('<div id=\\'leadsID_'+data[5]+'\\' class=\\'leads-item\\'>'+middle+right)
                           .append('</div>');
                           return row; }"),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'), # reset
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }') # bind select boxes to Shiny
      ),
      callback = JS(""),
      extensions = "Select",
      selection = "none"
    ),
    server = T)
    
    # TABLE OUTPUT FOR PREDEFINED LIST OF WORDS
    collections <- eventReactive(input$loadCollection, {
      print("Colections refresh")
        print("submittbutton")
      collectionsOutput <- gta_sql_get_value(sqlInterpolate(pool, "SELECT collection_id, collection_name FROM b221_collection_log;"))
      collectionsOutput <<- collectionsOutput
    })

    
    # COLLECT ALL CLICKS ON LEADS-ITEM
    observeEvent(input$submit, {
      # collect data => pass it to collectedData input
      shinyjs::runjs("collectData();")
    })
    
    
    # VALIDATE DATA, PASS IT TO DATABASE AND SHOW IT TO USER
    observeEvent(input$collectedData, {
      print("COLLETDATA")
      changes = jsonlite::fromJSON(input$collectedData)
      print(changes)
      validate( # validate if user is not submitting empty data
        validateSubmit(changes)
      )
      # UPDATE WITH NEW VALUES
      for(row in 1:nrow(changes)) {  
        id = changes[row,"id"]
        clicked = changes[row,"clicked"]
        gta_sql_update_table(sqlInterpolate(pool, "UPDATE bt_leads_core SET bin_check = true,
                                            bin_recovered = ?clickedVal, relevant = ?clickedVal, sent_out = false,
                                            evaluation = ?clickedVal WHERE lead_id = ?leadsID;",
                                            leadsID = id, clickedVal = clicked))
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

