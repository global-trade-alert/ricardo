# Define reactive values used in this app module up here, these will be passed automatically
# Don't forget to add these variables as function parameters as well
# SERVER

  load(file.path(paste0(path,"apps/deliver/data/GTA-COVID data.Rdata")), new_env <- new.env() )
  
  # preprocess data ----------------------------------DELETE THIS AFTER CONNECTING dlvr_pull_display
  new_env$covid.data <-
    new_env$covid.data %>%
    mutate(inst.export.barrier = ifelse(inst.export.barrier == TRUE, "export barrier", "")) %>%
    mutate(inst.import.barrier = ifelse(inst.import.barrier == TRUE, "import barrier", "")) %>%
    mutate(inst.domestic.subsidy = ifelse(inst.domestic.subsidy == TRUE, "domestic subsidy", "")) %>%
    mutate(inst.export.subsidy = ifelse(inst.export.subsidy == TRUE, "export subsidy", "")) %>%
    mutate(inst.other = ifelse(inst.other == TRUE, "other", "")) %>%
    mutate(inst.unclear = ifelse(inst.unclear == TRUE, "unclear", "")) %>%
    mutate(prd.med.con = ifelse(prd.med.con == TRUE, "medical consumables", "")) %>%
    mutate(prd.med.eqm = ifelse(prd.med.eqm == TRUE, "medical equipment", "")) %>%
    mutate(prd.med.drug = ifelse(prd.med.drug == TRUE, "medicines or drugs", "")) %>%
    mutate(prd.other = ifelse(prd.other == TRUE, "other", "")) %>%
    mutate(prd.food = ifelse(prd.food == TRUE, "food", "")) %>%
    mutate(prd.med.any = ifelse(prd.med.any == TRUE, "any medical product", "")) %>%
    mutate(products = paste(prd.med.con, prd.med.eqm, prd.med.drug, prd.other, prd.food, prd.med.any, sep=',')) %>%
    mutate(instruments = paste(inst.export.barrier, inst.import.barrier, inst.domestic.subsidy,
                               inst.export.subsidy, inst.other, inst.unclear, sep=',')) %>%
    select (-c(inst.export.barrier, inst.import.barrier, inst.domestic.subsidy,
               inst.export.subsidy, inst.other, inst.unclear, prd.med.con, prd.med.eqm,
               prd.med.drug, prd.other, prd.food, prd.med.any))
  
  new_env$covid.data$products <-
    new_env$covid.data$products %>%
    str_replace_all("\\,{2,}", ",") %>%
    str_replace_all("^\\,", "") %>%
    str_replace_all("\\,$", "")
  
  new_env$covid.data$products <-
    new_env$covid.data$products %>%
      str_replace_all("any medical product", "uncertain")
  
  new_env$covid.data$instruments <-
    new_env$covid.data$instruments %>%
    str_replace_all("\\,{2,}", ",") %>%
    str_replace_all("^\\,", "") %>%
    str_replace_all("\\,$", "")
  
  # create confirmation column with random values
  new_env$covid.data$confirmation <- as.character(sample(4, size = nrow(new_env$covid.data), replace = TRUE))
  new_env$covid.data <- 
    new_env$covid.data %>%
    mutate(confirmation = str_replace(confirmation, "1", "confirmed")) %>%
    mutate(confirmation = str_replace(confirmation, "2", "updated")) %>%
    mutate(confirmation = str_replace(confirmation, "3", "new")) %>%
    mutate(confirmation = str_replace(confirmation, "4", "deleted"))
  
  # create users involved column with random users
  new_env$covid.data$users <- as.character(sample(6, size = nrow(new_env$covid.data), replace = TRUE))
  new_env$covid.data <- 
    new_env$covid.data %>%
    mutate(users = str_replace(users, "1", "LG,PB")) %>%
    mutate(users = str_replace(users, "2", "PB,JF,KM,LG")) %>%
    mutate(users = str_replace(users, "3", "JF")) %>%
    mutate(users = str_replace(users, "4", "DR,JF")) %>%
    mutate(users = str_replace(users, "5", "OR")) %>%
    mutate(users = str_replace(users, "6", "KM"))
  
  new_env$covid.data <- 
    new_env$covid.data[c("confirmation","entry.id", "users", "entry.type","country","initial.assessment",
                         "gta.intervention.type","date.announced","date.implemented","date.removed",
                         "description","source", "products","instruments")]
  
  new_env <<- new_env

  # ----------------------------------------------------------------------------

deliverserver <- function(input, output, session, user, app, prm, ...) {
  

# Pull data ---------------------------------------------------------------
  names <- reactive({
    output <- dlvr_pull_display(last.deliverable = "2020-07-29 12:20:13")
    output$confirmation.status <- as.character(sample(4, size = nrow(output), replace = TRUE))
    output <- output %>%
      mutate(confirmation.status = str_replace(confirmation.status, "1", "confirmed")) %>%
      mutate(confirmation.status = str_replace(confirmation.status, "2", "updated")) %>%
      mutate(confirmation.status = str_replace(confirmation.status, "3", "new")) %>%
      mutate(confirmation.status = str_replace(confirmation.status, "4", "deleted"))
    output$gta.intervention.type = "GTA intervention type"
    output$original.description = NULL
    output$original.title = NULL
    output$users = "Users"
    output$product.group.name <- gsub(" ; ",",",output$product.group.name)
    output$intervention.type.name <- gsub(" ; ",",",output$intervention.type.name)
    # output$english.description = "Description"
    output <- output %>%
      select(confirmation.status,
             hint.id,
             users,
             documentation.status,
             is.official,
             jurisdiction.name,
             assessment.name,
             gta.intervention.type,
             announcement.date,
             implementation.date,
             removal.date,
             english.description,
             url,
             product.group.name,
             intervention.type.name,
             everything())
    change_attribute_table <<- data.frame(name=colnames(output), index=seq(0,length(output)-1,1))
    output <- output
  })
  
  observe({
    products_unique <-
      gta_sql_get_value("SELECT product_group_name FROM b221_product_group_list")
    instruments_unique <-
      gta_sql_get_value("SELECT intervention_type_name FROM b221_intervention_type_list")
    jurisdiction_unique <-
      gta_sql_get_value("SELECT jurisdiction_name FROM gta_jurisdiction_list")
    assessment_unique <-
      gta_sql_get_value("SELECT assessment_name FROM b221_assessment_list")
    discard_reason <- list('reason1', 'reason2', 'reason3', 'reason4', 'reason5', 'reason6')
    
    # print(list(Products = products_unique,
    #            Instruments = instruments_unique,
    #            Jurisdiction = jurisdiction_unique,
    #            'Initial assessment' = assessment_unique,
    #            discard_reason = discard_reason))
    
    session$sendCustomMessage('data_gta', shiny:::toJSON(list(Products = products_unique,
                                                              Instruments = instruments_unique,
                                                              Jurisdiction = jurisdiction_unique,
                                                              'Initial assessment' = assessment_unique,
                                                              discard_reason = discard_reason)))
  })
  
  ns <- NS("deliver")
  
  

# Output Table ------------------------------------------------------------
  output$deliverTable <- DT::renderDataTable(DT::datatable(
    data = names(), #retrieve_data(),
    colnames = c('Confirmation Status' = 1, 'Entry ID' = 2, 'Users' = 3, 'Documentation status' = 4, 'Is official?' = 5, 'Jurisdiction' = 6, 'Initial assessment' = 7, 'GTA intervention type' = 8, 
                 'Announcement date' = 9, 'Implementation date' =10, 'Removal date' = 11, 'Description' = 12,
                 'Source' = 13, 'Products' = 14, 'Instruments' = 15),
    
    rownames = FALSE,
    escape = FALSE,
    options = list(
      rowId = JS("function(d) {
        return d[1];
      }"),
      pageLength = 300,
      scrollX = FALSE,
      
      deferRender = TRUE,
      scrollY = JS('window.innerHeight - 160'),
      scroller = TRUE,
      
      dom = 'Pfrtip', 
      
      searchPanes = list(
        cascadePanes = TRUE,
        viewTotal = TRUE,
        emptyMessage = "<i><b>no products</b></i>",
        dtOpts = list(
          select = list(
            style = "multi"
          )
        )
      ),
      
      language = list(
        search = "_INPUT_",
        searchPlaceholder = "Filter",
        searchPanes = list(
          count = '{total} found',
          countFiltered = '{shown} / {total}'
        )
      ),
      select = list(
        searchPanes = list(
          style = 'multi'
        ),
        style='api'
      ),
      autoWidth = FALSE,
      columnDefs = list(
       # Hide table columns
          list(
            visible = FALSE,
            targets = c(15:33)
          ),
       # set columns widths
          list(  # Confirmation status
            targets = 0,
            className = "dt-head-left status"
          ),
          list(  # Hint ID
            targets = 1,
            className = "dt-head-left smallPadding entry-id"
          ),
          list(  # Users
            targets = 2,
            className = "dt-head-left smallPadding users"
          ),
          list( # Documentation status
            targets = 3,
            className = "dt-head-left smallPadding documentation-status"
          ),
         list( # Documentation status
           targets = 4,
           className = "dt-head-left smallPadding is-official"
         ),
          list( # Jurisdiction
            targets = 5,
            className = "dt-head-left smallPadding jurisdiction"
          ),
          list( # Initial assessment
            targets = 6,
            className = "dt-head-left smallPadding assessment"
          ),
          list( # GTA intervention type
            targets = 7,
            className = "dt-head-left smallPadding type"
          ),
          list( # Announcement date
            targets = 8,
            className = "dt-head-left smallPadding announcement-date"
          ),
          list(  # Implementation date
            targets = 9,
            className = "dt-head-left smallPadding implementation-date"
          ),
          list(  # Removal date
            targets = 10,
            className = "dt-head-left smallPadding removal-date"
          ), # 43%
          list(  # Description
            targets = 11,
            className = "dt-head-left smallPadding description"
          ),
          list(  # Source,
            targets = 12,
            className = "dt-head-left smallPadding source"
          ),
          list(  # Products
            targets = 13,
            className = "dt-head-left smallPadding products"
          ),
          list(  # Instruments
            targets = 14,
            className = "dt-head-left smallPadding instruments"
          ),
          list(targets = '_all',
               createdCell = JS("function (td, cellData, rowData, row, col) {
                                  $(td).css('padding', '0px')
                                }
                                ")),
        list(targets = c(0:9),
             className = 'dt-center'),
        list(targets = 0,
             render = JS("function (data, type, row){

                            if (type === 'sp') {
                              return data;
                            }
                            
                            let accepted = !/confirmed|deleted/gi.test(data) ? '<div class=\"accept\" title=\"Confirm entry\"><img src=\"www/deliver/accept.svg\"/></div>' : '<div></div>',
                                deleted = '<div class=\"delete\" title=\"Remove entry\"><img src=\"www/deliver/delete.svg\"/></div>',
                                restore = /deleted/gi.test(data) ? '<div class=\"restore\" title=\"Recover entry\"><img src=\"www/deliver/restore.svg\"/></div>' : 
                                '<div></div>',
                                edit = '<div class=\"edit\" title=\"Edit Entry\"><img src=\"www/deliver/edit.svg\"/></div>',
                                duplicates = '<div class=\"duplicate\" title=\"Remove duplicates\"><img src=\"www/deliver/duplicate.svg\"/></div>',
                                duplicates_remove = '<input type=\"checkbox\" class=\"duplicates-remove\">';
                                
                            if (data == 'confirmed') {
                              var status = '<span class=\\'material-icons\\'>check</span>';
                            } else if (data == 'deleted') {
                              var status = '<span class=\\'material-icons\\'>delete</span>';
                            } else if (data == 'updated') {
                              var status = '<span class=\\'material-icons\\'>update</span>';
                            } else if (data == 'new') {
                              var status = '<span class=\\'material-icons\\'>fiber_new</span>';

                            }
                            
                            let output = `<div class=\"status-row\">
                                              <div class=\\'button-wrap\\'><div class=\"buttons-column\">${accepted + restore + deleted + edit + duplicates + duplicates_remove}</div></div>
                                              <div class=\"status-column \">
                                                <div class=\"status-label ${data}\" alt=\\'${data}\\'>${status}</div>
                                              </div>
                                          </div>`;
                                   
                            return output;
               }"),
             searchPanes = list(
               orthogonal = 'sp'
             )
        ),
        list(targets = 2,
             render = JS("function (data, type, row){
                            let users = data.split(',').map(d =>`<div class=\"usr-label\">${d}</div>`).join('');
                             return `<div class=\"box-usr-label\">
                                        ${users}
                                     </div>`
               }")
        ),
        list(targets = 5,
             render = JS("function (data, type, row){
                            if (type === 'sp') {
                              return data.split(',')
                            }
                            return data;
               }"),
             searchPanes = list(
               orthogonal = 'sp'
             )
        ),
        list(targets = 8,
             render = JS("function(data,type,row){
                            if (data != null){
                                return `<div class=\"ann-date\">${data}</div>` 
                            } else {
                                return ''
                            }
               }")
        ),
        list(targets = 14,
             render = JS("function (data, type, row) {
                          if (data != null) {
                            if (type === 'sp') {
                              return data.split(',')
                            }
                            let output = data.split(',').map(d => `<div class=\"instr-label\">${d}</div>`);

                            let all = [];
                            
                            for (let i in output){
                              all.push(output[i])
                            }
                            
                            all = `<div class = \"col-left\">${all.join('')}</div>`;

                            return data != '' ? `<div class=\"box-item-label\">${all}</div>` : '';
                          } else {
                          return '';
                          }
                }"),
             searchPanes = list(
               orthogonal = 'sp',
               dtOpts = list(
                 initComplete = JS("function(){
                          $('.dtsp-searchPane:visible').removeClass('dtsp-columns-3');
                          $('.dtsp-searchPane:visible').addClass('dtsp-columns-5');
                          
                           // Add toggle button to bottom of search panes
                          let newNode = document.createElement('div');
                          newNode.setAttribute('id', 'search-pane-toggle-button');
                          let innerNode = document.createElement('span');
                          innerNode.setAttribute('class','material-icons');
                          innerNode.innerHTML = 'expand_less';
                          newNode.appendChild(innerNode);
                          let referenceNode = $('#deliver-deliverTable .dtsp-panesContainer .dtsp-titleRow')[0];
                          referenceNode.appendChild(newNode);
                          
                          // Add export .xlsx to bottom of search panes
                          let export_div = document.createElement('div');
                          Object.assign(export_div, {
                            className: 'search-pane-export-xlsx',
                            title: 'export as .xlsx',
                            onclick: function(){ buttonsClicks['initializeSaveMode']();  }
                          });
                          let img = document.createElement('img');
                          Object.assign(img, {
                            src: 'www/deliver/save-xlsx.svg',
                            id: 'export-svg'
                          })
                          export_div.appendChild(img);
                          referenceNode.appendChild(export_div);
                          
                          searchPaneUI();
                          
                                                                                    
                          let pagination = $('#DataTables_Table_0_paginate');
                          console.log('PAGINATION');
                          console.log(pagination);
                          pagination.appendTo('.dtsp-panesContainer .dtsp-titleRow');
                         
                   }"),
                 drawCallback = JS("function(settings){
                                     const api = this.api();
                   }"),
                 predrawCallback = JS("function(settings){
                                     const api = this.api();

                   }"),
                 createdRow = JS("function(row, data, dataIndexcells){

                                   }")
               )
             )
        ),
        list(targets = 13,
             render = JS("function (data, type, row) {
                            if (data != null) {
                              if (type === 'sp') {
                                  return data.split(',')
                              } 
                              let output = data.split(',').map(d => `<div class=\"prd-label\">${d}</div>`)//.join('');
                              
                              let all = [];
  
                              for (let i in output){
                                all.push(output[i])
                              }
                              
                              all = `<div class = \"col-left\">${all.join('')}</div>`;
                              return data != '' ? `<div class=\"box-item-label\">${all}</div>` : '';
                            } else {
                            return '';
                            }
                }"),
             searchPanes = list(
               orthogonal = 'sp'
             )
        ),
        list(targets = 12,
             render = JS("function(data, type, row, meta){
                          data = data.replace(/(https?[^ ]+)/gi, '<a href=\"$1\" target=\"_blank\">$1</a>');

                          let output = `<div class=\"source-less\">${data}</div>`;

                          return output
               }")),
        list(targets = 11,
             render = JS("function(data, type, row, meta){
                          let output = `<div class=\"description-less\">${data}</div>`;

                          return output
               }")),
        
        # searchPanes extension
        list(
          searchPanes = list(
            show = FALSE
          ),
          targets = c(1:3,7:12)#,12:22
        )
      ),
      
      initComplete = JS("function(settings) {
                            const api = this.api();
                            //$('#hide').css({'display': ''}); //make table visible only after adjustments
                            settings._searchPanes.regenerating = true // allow recalculation of searchPanes

      }"),
      
      infoCallback = JS("function(settings, start, end, max, total, pre){
                                const api = this.api();

      }"),
      
      #clear display before creating content
      preDrawCallback = JS("function(settings){
                           const api = this.api();
                           //api.$('.more-less').remove();
                           $('#hide').length == 0 ?
                           $('#DataTables_Table_0').wrap('<div id=\"hide\" style=\"display: none\"</div>'): //make table invisible before adjustments
                           0;
                           
                          //$('#DataTables_Table_0_wrapper thead').wrap('<div id=\"hide_1\" style=\"display: none\"</div>')
                           // check if text length is bigger than tr height
                            function isEllipsisActive($jQueryObject) {
                                return ($jQueryObject[0].offsetHeight < $jQueryObject[0].scrollHeight);
                            }
                            
                           new Promise((resolve, reject) => {
                                $('#DataTables_Table_0').resize() // adjust columns widths
                                resolve();
                            })
                            .then(() => {
                                $('#hide').css({'display': ''});
                                  api.$('.description-less').each(function(){ // all un-opened descriptions
                                  $(this)[0].setAttribute('style', `max-height:${$(this)[0].parentNode.scrollHeight}px;`);
                                  let id = api.row( $(this).closest('tr') ).id();
                                    if(isEllipsisActive($(this)) == true && $(this).siblings('.more-less').length == 0){
                                        $(this).parent('td').append(`<button id =\"toggle-description_${id}\"
                                                                          class=\"more-less\" onclick=\'showMorecontent(\"description\",${id})\'>
                                                                          <span class=\\'material-icons\\'>add_circle_outline</span></button>`)
                                    }
                                })
                              api.$('.source-less').each(function(){ // all un-opened sources
                                  // console.log($(this));
                                  // console.log($(this)[0].scrollHeight);
                                  // console.log($(this)[0].parentNode.scrollHeight);
                                  $(this)[0].setAttribute('style', `max-height:${$(this)[0].parentNode.scrollHeight}px;`);
                                  let id = api.row( $(this).closest('tr') ).id();
                                  if(isEllipsisActive($(this)) == true && $(this).siblings('.more-less').length == 0){
                                        $(this).parent('td').append(`<button id =\"toggle-source_${id}\"
                                                                          class=\"more-less\" onclick=\'showMorecontent(\"source\",${id})\'>
                                                                          <span class=\\'material-icons\\'>add_circle_outline</span></button>`)
                                    }
                                })
                            })

      }"),
      
      drawCallback = JS("function(settings){
                              const api = this.api();
                              
                               api.$('.status-label').each(function(){
                                  let type = $(this)[0].className;
                                  $(this).closest('tr').addClass(type.replace('status-label ',''))
                                })
                              
                              let data = api.rows( { page: 'current' } ).data();

                        }"),
      
      rowCallback = JS("function(row, data){

      }"),
      
      createdRow = JS("function(row, data, dataIndex, cells){
      
                      $(row).find('.buttons-column').each(function(){
                            let status = data[0],
                                that = $(this),
                                id = data[1];
                                
                              $(that).children().each(function(){
                                if ($(this).attr('class') != 'duplicates-remove')
                                $(this).on('click', function() { buttonsClicks[$(this).attr('class')](status, id) })
                              })
                        })

      }")
    ),
    class = "row-border compact",
    extensions = c("Select", 'SearchPanes'),
    selection = "none"
    ),
  server = F)
  
  observeEvent(input$saveXlsx, {
    export <- jsonlite::fromJSON(input$saveXlsx)

    print(export)
    export <- export %>%
      mutate('Product: medical consumables' = if('Products' %in% names(.)) str_detect(Products, 'medical consumables') else NULL,
             'Product: Medical equipment' = if('Products' %in% names(.)) str_detect(Products, 'medical equipment.') else NULL,
             'Product: Medicines or drugs' = if('Products' %in% names(.)) str_detect(Products, 'medicines or drugs') else NULL,
             'Product: Food' = if('Products' %in% names(.)) str_detect(Products, 'food') else NULL,
             'Product: Any medical product' = if('Products' %in% names(.)) str_detect(Products, 'uncertain') else NULL,
             'Product: other' = if('Product' %in% names(.)) str_detect(Products, 'other') else NULL,
             'Is export barrier' = if('Instruments' %in% names(.)) str_detect(Instruments, 'export barrier') else NULL,
             'Is import barrier' = if('Instruments' %in% names(.)) str_detect(Instruments, 'import barrier') else NULL,
             'Domestic subsidy' = if('Instruments' %in% names(.)) str_detect(Instruments, 'domestic subsidy') else NULL,
             'Export subsidy' = if('Instruments' %in% names(.)) str_detect(Instruments, 'export subsidy') else NULL) %>%
      select(!any_of(c('Products', 'Instruments')))
    
    data_export <<- list("WB data" = export, "Notes" = c('Data as available on CURRENTTIME.'))
    
    runjs("$('#deliver-downloadXlsx')[0].click();
           $('.overlay').click();")
  })
  
  output$downloadXlsx <- downloadHandler(
    filename = function() {
      paste0("test", ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(data_export, file = file)
    }
  )
 
  observeEvent(input$changeData, {
    changedData <<- jsonlite::fromJSON(input$changeData)
    changedData <- merge(changedData, change_attribute_table, by="index", keep.x=T)
    change.id = as.numeric(unique(changedData$hintId))
    
    if ("jurisdiction.name" %in% changedData$name) { jurisdiction <- changedData[changedData$name == "jurisdiction.name",] }
    if ("intervention.type.name" %in% changedData$name) { instrument <- changedData[changedData$name == "intervention.type.name",] }
    if ("product.group.name" %in% changedData$name) { product <- changedData[changedData$name == "product.group.name",] }
    
    b221_hint_change_attribute(change.id=change.id,
                               is.intervention=F,
                               intervention.modifiable=T,
                               modify.assessment=switch("assessment.name" %in% changedData$name, changedData$dataNew[changedData$name=="assessment.name"], NULL),
                               modify.date.announced=switch("announcement.date" %in% changedData$name, changedData$dataNew[changedData$name=="announcement.date"], NULL),
                               modify.date.implemented=switch("implementation.date" %in% changedData$name, changedData$dataNew[changedData$name=="implementation.date"], NULL),
                               modify.date.removed=switch("removal.date" %in% changedData$name, changedData$dataNew[changedData$name=="removal.date"], NULL),
                               modify.description=switch("english.description" %in% changedData$name, changedData$dataNew[changedData$name=="english.description"], NULL),
                               add.instrument=switch(exists("instrument"), switch(length(setdiff(strsplit(instrument$dataNew,",")[[1]],strsplit(instrument$dataOld,",")[[1]]))>0, setdiff(strsplit(instrument$dataNew,",")[[1]],strsplit(instrument$dataOld,",")[[1]]), NULL),NULL),
                               remove.instrument=switch(exists("instrument"), switch(length(setdiff(strsplit(instrument$dataOld,",")[[1]],strsplit(instrument$dataNew,",")[[1]]))>0, setdiff(strsplit(instrument$dataOld,",")[[1]],strsplit(instrument$dataNew,",")[[1]]), NULL), NULL),
                               add.product=switch(exists("product"), switch(length(setdiff(strsplit(product$dataNew,",")[[1]],strsplit(product$dataOld,",")[[1]]))>0, setdiff(strsplit(product$dataNew,",")[[1]],strsplit(product$dataOld,",")[[1]]), NULL), NULL),
                               remove.product=switch(exists("product"), switch(length(setdiff(strsplit(product$dataOld,",")[[1]],strsplit(product$dataNew,",")[[1]]))>0, setdiff(strsplit(product$dataOld,",")[[1]],strsplit(product$dataNew,",")[[1]]), NULL), NULL),
                               add.jurisdiction=switch(exists("jurisdiction"), switch(length(setdiff(strsplit(jurisdiction$dataNew,",")[[1]],strsplit(jurisdiction$dataOld,",")[[1]]))>0, setdiff(strsplit(jurisdiction$dataNew,",")[[1]],strsplit(jurisdiction$dataOld,",")[[1]]), NULL), NULL),
                               remove.jurisdiction=switch(exists("jurisdiction"), switch(length(setdiff(strsplit(jurisdiction$dataOld,",")[[1]],strsplit(jurisdiction$dataNew,",")[[1]]))>0, setdiff(strsplit(jurisdiction$dataOld,",")[[1]],strsplit(jurisdiction$dataNew,",")[[1]]), NULL), NULL)
                               )
    
  })
  
}