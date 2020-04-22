# SERVER
oscserver <- function(input, output, session, user, app, prm, ...) {
  
  ns <- NS("osc")

 
  # UPDATE DATE OF CREATION OF APP.R WHEN CLOSING, PREVENTS CACHING OF CSS AND JS
  onStop(function() {
    p <- paste0(getwd(), path, "/code/app.R")
    # Update file 'date creation'
    Sys.setFileTime(p, Sys.time())
  })
  
  observe({
    click("loadMoreLeads")
  })
  
  # OUTPUT LEADS TABLE
  
  output$leadsTable <- DT::renderDataTable({

    DT::datatable(
    names(),
    rownames = FALSE,
    escape = FALSE,
    # USE TO SELECT WHEN SEARCH FIELD IS ACTIVE
    options = list(
      pageLength = 50,
      columnDefs = list(list(visible = FALSE, targets = c(0,1,2,3,4,5,6,7,8)), list(sortable=FALSE, targets = c(0))),
      language = list(
      zeroRecords = "No more leads available."),
      rowCallback = JS("function ( row, data ) {
                            let comments = data[6] == '' ? 'No comments available' : data[6];
                            
                            let readMore, image;
                            if (comments.length > 300) {
                                readMore = ' readMore';
                                image = 'www/expand.svg';
                            } else {
                                readMore = '';
                                image = 'www/expandgrey.svg';
                            }
                            
                            
                            comments = '<h5>Previous comments: </h5><p>' + comments.split(';').join('<br>') + '</p>';
                            
                            let country = '<div class = \"country\" id=\"country_\' + data[0] + '\"><h5>Implementer country(ies): </h5><p>' + data[1] + '</p></div>';
                            let official_url = data[2];
                            let news_url = data[3];
                            let consultancy_url = data[4];
                            let other_url = data[5];
                            let file_path = data[7];
                            let new_comments = data[8];
                            
                            let submit = '<button id=\\'submit_'+data[0]+'\\' type=\"button\" disabled class=\"btn btn-default action-button\">Submit data!</button>';
                            
                            let top_left_col = '<div class=\\'top-left-col\\'>' + country + news_url + consultancy_url + '</div>';
                            let top_right_col = '<div class=\\'top-right-col\\'>' + other_url + official_url + file_path + '</div>';
                            let middleTop = '<div class=\\'top-row\\'><div class=\\'country-lead\\'>'+ top_left_col + '</div>'+'<div class=\\'acting-agency\\'>'+ top_right_col + '</div></div>';

                            var middleMiddle = '<div class=\\'middle-row\\'><div class=\\'act-description\\'>'+comments + '</div><div class=\\'new-comment\\'>' + new_comments + '</div><div class=\\'gradient-bottom\\'><div class=\\'gradient-inner\\'></div></div></div>';
                            var middleBottom = '<div class=\\'bottom-row no-touch\\'><div class=\\'show-more no-touch\\'><img src=\\''+image+'\\' class=\\'svg no-touch\\'></div><div class=\\'pop-up no-touch\\'>' + submit + '</div></div>'; //'<div class=\\'pop-up no-touch\\'><img src=\\'www/popup.svg\\' class=\\'svg no-touch\\'></div></div>';
                            var middle = '<div class=\\'middle-col'+readMore+'\\'>'+middleTop+middleMiddle+middleBottom+'</div>';
                            var right = '<div class=\\'right-col\\'></div>';
                           $(row)
                           .append('<div id=\\'leadsID_'+data[0]+'\\' class=\\'leads-item\\'>'+middle+right)
                           .append('</div>');
                           return row; }"),
      initComplete = JS("function(settings) {
                          $('[id^=country]').selectize({
                            placeholder: 'Choose country...'
                          });
                          $('[id^=official]').selectize({
                            placeholder: 'Choose official url...',
                            create: true
                          });
                          $('[id^=news]').selectize({
                            placeholder: 'Choose news url...',
                            create: true
                          });
                          $('[id^=consultancy]').selectize({
                            placeholder: 'Choose consultancy url...',
                            create: true
                          });
                          $('[id^=other]').selectize({
                            placeholder: 'Choose other url...',
                            create: true
                          });
                          $('[id^=file]').selectize({
                            placeholder: 'Choose file...',
                            create: true
                          });
                        }"),
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'), # reset
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }') # bind select boxes to Shiny
    ),
    extensions = "Select",
    selection = "none",
    callback = JS('
          table.on("click.dt","tr", function() {
                      var data=table.row(this).data();
          });' )
    )
    },
    server = T)


  # INITIAL DATA - FILL SQL HERE --------------------------------------------------------------------------------------------------------
  
  names <- eventReactive(input$loadMoreLeads, {
    print("LEADS REFRESH")
    shinyjs::runjs("window.scrollTo(0, 0);")
    
    hint_id <- c(100, 120, 160, 170, 180)
    country <- c('Australia;Ukraine', 'Russian Federation', 'USA;Argentina', 'China;South Korea', 'Canada')
    official_url <- c('http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                      'http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                      'http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                      'http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                      '')
    news_url <- c('http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf;http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                  'http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf;http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf;
                  http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                  'http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                  'http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                  'http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf')
    consultancy_url <- c('http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf;http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                         'http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf;http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                         'http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                         'http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                         'http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf')
    other_url <- c('http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf;http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                   'http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                   'http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                   'http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf;http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf',
                   'http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf;http://dgft.gov.in/Exim/2000/TN/TN18/Trade%20Notice%2016.pdf')
    backlog_of_comments <- c('Some comment;Some comment','','','Some comment','Some comment;Some comment;Some comment')
    
    # --------------------------------------------------------------------------------------------------------------------------
    
    osc.output <- data.frame(hint_id, country, official_url, news_url, consultancy_url, other_url, backlog_of_comments)
    
    # INITIALIZING SELECT BOXES UIs
    # osc.output$country <- apply(osc.output,1, function(x){
    #                     as.character(selectizeInput(paste0('country_',x['hint_id']),
    #                                                 selected = unlist(str_split(x['country'],';')),
    #                                                 label = 'Implementing country',
    #                                                 choices = country.list,
    #                                                 multiple = TRUE)
    #                     )
    #                   })
    osc.output$official_url <- apply(osc.output,1, function(x){
      as.character(textInput(paste0('official_',x['hint_id']), 
                             'Official url', 
                             value = x['official_url'], 
                             width = NULL,
                             placeholder = 'Type official url...')
      )
    })
    
    osc.output$news_url <- apply(osc.output,1, function(x){
      as.character(selectizeInput(paste0('news_',x['hint_id']),
                                  selected = NULL,
                                  label = 'News url',
                                  choices = unlist(str_split(x['news_url'],';')),
                                  multiple = TRUE)
      )
    })
    
    osc.output$consultancy_url <- apply(osc.output,1, function(x){
      as.character(selectizeInput(paste0('consultancy_',x['hint_id']),
                                  selected = NULL,
                                  label = 'Consultancy url',
                                  choices = unlist(str_split(x['consultancy_url'],';')),
                                  multiple = TRUE)
      )
    })
    
    osc.output$other_url <- apply(osc.output,1, function(x){
      as.character(selectizeInput(paste0('other_',x['hint_id']),
                                  selected = NULL,
                                  label = 'Other url',
                                  choices = unlist(str_split(x['other_url'],';')),
                                  multiple = TRUE)
      )
    })
    
  })
  
  osc.output$file_path <- apply(osc.output,1, function(x){
    as.character(
      fileInput(paste0('path', x['hint_id']),
                label = 'Upload files',
                multiple = TRUE,
                accept = NULL,
                width = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected")
    )
  })
  
  osc.output$comment <- apply(osc.output,1, function(x){
    as.character(textAreaInput(paste0('comment',x['hint_id']), h5("Add comment:"), 
                               value = "Enter text...", resize = 'vertical')
    )
  })
  
  # TABLE OUTPUT FOR PREDEFINED LIST OF WORDS
  names <- eventReactive(input$loadMoreLeads, {
      print("LEADS REFRESH")
      shinyjs::runjs("window.scrollTo(0, 0);")
      leads.output <- gta_sql_get_value(sqlInterpolate(pool, "SELECT country_lead, acting_agency, act_title_en, act_description_en, act_url, 
                                                              lead_id, collection_date, relevance_probability FROM bt_leads_core WHERE 
                                                              (bin_check = false AND relevant = false) ORDER BY relevance_probability DESC LIMIT 50;"))
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
                                    choices = product.list,
                                    multiple = TRUE)
        )
      })
      leads.output$select_intervention = apply(leads.output,1, function(x){
        as.character(selectizeInput(paste0('intervention_',x['lead.id']),
                                    label = 'Intervention Type',
                                    choices = intervention.list,
                                    multiple = TRUE)
        )
      })
      leads.output <<- leads.output
    })
    
    
    # OBSERVE SHINY JS CHECK LEADS EVENT FOR ITEMS PASSING SCREEN TOP
    observeEvent(input$checkLeads, {
      id <- as.numeric(gsub("leadsID_","", input$checkLeads))
      print(paste0("Scrolled: ",id))
      if (input$`selection-mode` == FALSE){ # check current state of the app
        gta_sql_update_table(sqlInterpolate(pool, "UPDATE bt_leads_core SET bin_check = true, bin_recovered = false, relevant = false, sent_out = false, evaluation = false WHERE lead_id = ?leadsID;", leadsID = id))
        gta_sql_update_table(sqlInterpolate(pool, "INSERT INTO bt_leads_checked VALUES (?leadsID, ?userID);", leadsID = id, userID = active.user.selected))
        }
      })

    # OBSERVE CLICKS ON LEADS-ITEM AND CHANGE DATABSE ENTRY ACCORDINGLY
    observeEvent(input$checkLeadsClick, {
     id <- as.numeric(gsub("leadsID_","", input$checkLeadsClick[2]))
     print(paste0("Clicked: ",id))
     if (input$`selection-mode` == FALSE){ # check current state of the app
       if (input$checkLeadsClick[1]==T) {
         gta_sql_update_table(sqlInterpolate(pool, "UPDATE bt_leads_core SET bin_check = true, bin_recovered = true, relevant = true, sent_out = false, evaluation = true WHERE lead_id = ?leadsID;", leadsID = id))
       } else if (input$checkLeadsClick[1]==F) {
         gta_sql_update_table(sqlInterpolate(pool, "UPDATE bt_leads_core SET bin_check = true, bin_recovered = false, relevant = false, sent_out = false, evaluation = false WHERE lead_id = ?leadsID;", leadsID = id))
       }
     }
    })
    
    # OBSERVE CLICKS ON LEADS-ITEM AND OPEN POPUP
    # observeEvent(input$countryPopUp, {
    #   removeUI(selector = ".removeui")
    #   print(as.numeric(gsub("leadsID_","", input$countryPopUp)))
    #   popupID <- as.numeric(gsub("leadsID_","", input$countryPopUp))
    #   currentPopUpID <<- popupID
    #   
    #   country.lead <- gta_sql_get_value(sqlInterpolate(pool, "SELECT country_lead FROM bt_leads_core WHERE lead_id = ?leadsID;", leadsID = popupID))
    #   
    #   insertUI(selector = "#countryPopUpPlaceholder",ui = tagList(
    #     tags$div(id=paste0("leadsPopUp_",popupID),
    #              class="removeui",
    #              selectInput("countryLeads",
    #                          label = "Change Lead Jurisdiction",
    #                          choices = country.list,
    #                          selected = country.lead),
    #              actionButton("submitCountryLead",
    #                           class="submitButton",
    #                           label = "Submit")
    #              )
    #     )
    #     )
    #   
    #   addClass(selector = paste0(".countryPopUp-pop-up"), class = "active")
    #   runjs("removePopUp();")
    #   })
    
    observeEvent(input$submitCountryLead, {
      gta_sql_update_table(sqlInterpolate(pool, "UPDATE bt_leads_core SET bin_check = true, bin_recovered = true, relevant = true, sent_out = false, evaluation = true WHERE lead_id = ?leadsID;", leadsID = currentPopUpID))
      gta_sql_update_table(sqlInterpolate(pool, "UPDATE bt_leads_core SET country_lead = ?newCountry WHERE lead_id = ?leadsID;", newCountry = input$countryLeads, leadsID = currentPopUpID))
      print(currentPopUpID)
      print(input$countryLeads)
      removeClass(selector = paste0(".countryPopUp-pop-up"), class = "active")
      
      })
    
    # OBSERVE CLICKS ON CHECK-BOX TO UPDATE DISMISSAL/SELECTION MODE
    observeEvent(input$`selection-mode`, {
      print(input$`selection-mode`)
      if(input$`selection-mode` == TRUE) shinyjs::enable("submit_all") else shinyjs::disable("submit_all")
    })
    
    # COLLECT ALL CLICKS ON LEADS-ITEM
    observeEvent(input$submit_all, {
      # collect data => pass it to collectedData input
      shinyjs::runjs("collectData();")
    })
    
    
    # VALIDATE DATA, PASS IT TO DATABASE AND SHOW IT TO USER
    observeEvent(input$collectedData, {rica
      changes = jsonlite::fromJSON(input$collectedData)
      print(changes)
      test_changes <<- changes
      validate( # validate if user is not submitting empty data
        validateSubmit(session, changes, input)
      )
      # for(row in 1:nrow(changes)) {
      #   id = changes[row,"id"]
      #   clicked = changes[row,"clicked"]
      #     gta_sql_update_table(sqlInterpolate(pool, "UPDATE bt_leads_core SET bin_check = true,
      #                                       bin_recovered = ?clickedVal, relevant = ?clickedVal, sent_out = false,
      #                                       evaluation = ?clickedVal WHERE lead_id = ?leadsID;",
      #                                       leadsID = id, clickedVal = clicked))
      # }

    })
    
    observe({
      insertUI(selector = "#loadMoreButton", ui = tagList(
        actionButton(ns("loadMoreLeads"),
                     "Refresh",
                     class="blue")
      ))
    })
    

}


