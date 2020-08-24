# Define reactive values used in this app module up here, these will be passed automatically
# Don't forget to add these variables as function parameters as well
# SERVER

deliverserver <- function(input, output, session, user, app, prm, ...) {
  
  
  # Pull data ---------------------------------------------------------------
  names <- eventReactive(input$lastDeliverable, {
    print("create table")
    output <- dlvr_pull_display(last.deliverable = paste0(input$lastDeliverable, " 00:00:00"))
    output_test1 <<- output
    output$original.description = NULL
    output$original.title = NULL
    output$users = "Users"
    output$product.group.name <- gsub(" ; ",",",output$product.group.name)
    output$intervention.type.name <- gsub(" ; ",",",output$intervention.type.name)
    # output$english.description = "Description"
    output <- output %>%
      select(confirm.status,
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
    output_test <<- output
  })
  
  
  observe({
    products_unique <-
      gta_sql_get_value("SELECT DISTINCT product_group_name FROM b221_product_group_list;")
    instruments_unique <-
      gta_sql_get_value("SELECT DISTINCT intervention_type_name FROM b221_intervention_type_list;")
    jurisdiction_unique <-
      gta_sql_get_value("SELECT DISTINCT jurisdiction_name FROM gta_jurisdiction_list;")
    assessment_unique <-
      gta_sql_get_value("SELECT DISTINCT assessment_name FROM b221_assessment_list;")
    discard_reason <- gta_sql_get_value("SELECT DISTINCT discard_reason_name FROM bt_discard_reason_list;")
    
    # print(list(Products = products_unique,
    #            Instruments = instruments_unique,
    #            Jurisdiction = jurisdiction_unique,
    #            'Initial assessment' = assessment_unique,
    #            discard_reason = discard_reason))
    
    runjs('$("body").addClass("initial");')
    
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
        emptyMessage = "<i><b>no data</b></i>",
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
          targets = c(15:47)
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
        list(targets = 1,
             render = JS("function (data, type, row){
                            let gta_id = row[47];
                            return gta_id == null ? data : gta_id;
             }")),
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
                              return data != null ? data.split(' ; ') : '';
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
                            if (type === 'sp') {
                              return data != null ? data.split(',') : '';
                            }
                            if (data == null){
                              return '';
                            } else {
                              let output = data.split(',').map(d => `<div class=\"instr-label\">${d}</div>`);
  
                              let all = [];
                              
                              for (let i in output){
                                all.push(output[i])
                              }
                              
                              all = `<div class = \"col-left\">${all.sort().join('')}</div>`;
  
                              return data != '' ? `<div class=\"box-item-label\">${all}</div>` : '';
                            }

                }"),
             searchPanes = list(
               orthogonal = 'sp',
               dtOpts = list(
                 initComplete = JS("function(){
                          console.log('RUND INITCOMPLETE SEARCH PANES');  
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
                          
                          // Add switch-deleted button to bottom of search panes
                          let switch_div = document.createElement('div');
                          switch_div.innerHTML = `<label class=\"toggle-deleted\" for=\"toggle-deleted-input\">\
                                                  <input type=\"checkbox\" checked=\"checked\" id=\"toggle-deleted-input\"\
                                                    onclick=\'buttonsClicks[\"switchDeleted\"]();\'>\
                                                  <span class=\"control\"></span>\
                                                </label>\
                                                <p>Show deleted</p>`;
                          Object.assign(switch_div, {
                            className: 'search-pane-switch-deleted',
                            title: 'add/remove deleted hints'
                          });
                          referenceNode.appendChild(switch_div);
                          
                          
                                                                                    
                          let pagination = $('.dataTables_paginate.paging_simple_numbers');
                          pagination.appendTo('.dtsp-panesContainer .dtsp-titleRow');
                          
                          if ($('body').hasClass('initial')) {
                            searchPaneUI();
                            $('body').removeClass('initial');
                          } 
                         
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
                            if (type === 'sp') {
                                return data != null ? data.split(',') : '';
                            } 
                            if (data == null){
                              return ''
                            } else {
                              let output = data.split(',').map(d => `<div class=\"prd-label\">${d}</div>`)//.join('');
                              let all = [];
  
                              for (let i in output){
                                all.push(output[i])
                              }
                              
                              all = `<div class = \"col-left\">${all.sort().join('')}</div>`;
                              return data != '' ? `<div class=\"box-item-label\">${all}</div>` : '';
                            }
                }"),
             searchPanes = list(
               orthogonal = 'sp'
             )
        ),
        list(targets = 12,
             render = JS("function(data, type, row, meta){
                          data = data != null ? data.replace(/(https?[^ ]+)/gi, '<a href=\"$1\" target=\"_blank\">$1</a>') : '';
                          return `<div class=\"source-less\">${data}</div>`;
               }")),
        list(targets = 11,
             render = JS("function(data, type, row, meta){
                          let output = `<div class=\"description-less\">${data == null ? '' : data}</div>`;
                          return output
               }")),
        
        # searchPanes extension
        list(
          searchPanes = list(
            show = FALSE
          ),
          targets = c(2:4,7:46)#,12:22
        )
      ),
      
      initComplete = JS("function(settings) {
                            const api = this.api();
                            //$('#hide').css({'display': ''}); //make table visible only after adjustments
                            settings._searchPanes.regenerating = true // allow recalculation of searchPanes
                            toggleConflict();
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
                        
                  if (data[36] == 1) {
                    if (data[25] != null) { var assessment = data[25]; } else {var assessment = ''; }
                    if (data[26] != null) { var types = data[26]; } else {var types = ''; }
                    if (data[27] != null) { var announcement = data[27]; } else {var announcement = ''; }
                    if (data[28] != null) { var implementation = data[28]; } else {var implementation = ''; }
                    if (data[29] != null) { var removal = data[29]; } else {var removal = ''; }
                    if (data[30] != null) { var jurisdiction = data[30]; } else {var jurisdiction = ''; }
                    if (data[31] != null) { var product = data[31]; } else {var product = ''; }
                    if (data[32] != null) { var description = data[32]; } else {var description = ''; }
                    if (data[33] != null) { var title = data[33]; } else {var title = ''; }
                    if (data[34] != null) { var relevance = data[34]; } else {var relevance = ''; }
                    if (data[35] != null) { var url = data[35]; } else {var url = ''; }
                    
                    var conflict = '<div class=\\'conflict\\'><p class=\\'title\\'>Conflict</p><div class=\\'conflict-info\\'>'+assessment+types+announcement+implementation+removal+jurisdiction+product+description+title+relevance+url+'</div>';
                  } else {
                    var conflict = '';
                  }  
                  
                
                  $(row)
                  .append('<div class=\\'break\\'></div>'+conflict);
                  return row;
      }")
    ),
    class = "row-border compact",
    extensions = c("Select", 'SearchPanes'),
    selection = "none"
  ),
  server = F)
  
  # EXPORT XLSX ----------------------------------------------------------------
  observeEvent(input$saveXlsx, {
    exportCols <- jsonlite::fromJSON(input$saveXlsx)
    exportCols <- merge(exportCols, change_attribute_table, by="index", keep.x=T) %>%
      rename('name_new' = name.x, 'name_old' = name.y)


    subset_prod = NULL
    subset_instr = NULL
    if(exportCols %>% filter(name_old == 'product.group.name') %>% nrow() > 0) {
      subset_prod = c('Product: medical consumables', 'Product: Medical equipment', 'Product: Medicines or drugs',
                      'Product: Food', 'Product: Any medical product', 'Product: other')
    }
    if(exportCols %>% filter(name_old == 'intervention.type.name') %>% nrow() > 0) {
      subset_instr = c('Is export barrier', 'Is import barrier', 'Domestic subsidy', 'Export subsidy')
    }
    subset = c(subset_prod, subset_instr)
    
      output_xlsx <- dlvr_pull_display(last.deliverable = paste0(lubridate::floor_date(lubridate::now(), "second")))
      output_xlsx <- output_xlsx %>%
        filter(relevance == 1)
      output_xlsx <- output_xlsx %>%
        mutate('Product: medical consumables' = ifelse(str_detect(`product.group.name`, 'medical consumables'), 'TRUE', 'FALSE'),
               'Product: Medical equipment' = ifelse(str_detect(`product.group.name`, 'medical equipment'), 'TRUE', 'FALSE'),
               'Product: Medicines or drugs' = ifelse(str_detect(`product.group.name`, 'medicines or drugs'), 'TRUE', 'FALSE'),
               'Product: Food' = ifelse(str_detect(`product.group.name`, 'food'), 'TRUE', 'FALSE'),
               'Product: Any medical product' = ifelse(str_detect(`product.group.name`, 'uncertain'), 'TRUE', 'FALSE'),
               'Product: other' = ifelse(str_detect(`product.group.name`, 'other'), 'TRUE', 'FALSE'),
               'Is export barrier' = ifelse(str_detect(`intervention.type.name`, 'export barrier'), 'TRUE', 'FALSE'),
               'Is import barrier' = ifelse(str_detect(`intervention.type.name`, 'import barrier'), 'TRUE', 'FALSE'),
               'Domestic subsidy' = ifelse(str_detect(`intervention.type.name`, 'domestic subsidy'), 'TRUE', 'FALSE'),
               'Export subsidy' = ifelse(str_detect(`intervention.type.name`, 'export subsidy'), 'TRUE', 'FALSE'),
               hint.id = ifelse(is.na(gta.id), hint.id, gta.id)) %>%
        select(c(exportCols$name_old, subset))
      
      setnames(output_xlsx, exportCols$name_old, exportCols$name_new)
      output_xlsx <- output_xlsx %>%
        select(!any_of(c('Products', 'Instruments')))
    data_export <<- list("WB data" = output_xlsx, "Notes" = c('Data as available on CURRENTTIME.'))
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
    
    # Prevent feeding empty attributes to b221_hint_change_attribute
    changedData <- changedData %>%
      mutate_at(.vars = 'dataNew', .funs = function(x){
        if (x == '') {
          x = NULL
        }
        return(x)
      })
    
    b221_hint_change_attribute(change.id=change.id,
                               user.id = user$id, 
                               is.superuser=ifelse(1 %in% user$group, TRUE, FALSE),
                               is.intervention=ifelse(unique(changedData$isIntervention)==1, TRUE, FALSE),
                               intervention.modifiable=T,
                               modify.assessment=switch("assessment.name" %in% changedData$name, changedData$dataNew[changedData$name=="assessment.name"], NULL),
                               modify.is.official=switch("is.official" %in% changedData$name, changedData$dataNew[changedData$name=="is.official"], NULL),
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
  
  # CONFIRM HINT ---------------------------------------------------------------
  observeEvent(input$confirmHint, {
    confirmedHint <<- jsonlite::fromJSON(input$confirmHint)
    print(confirmedHint)
    confirmedHint_classifications <- sprintf(paste0("SELECT DISTINCT change_ids.hint_id, ht_ass.ass_classification, ht_int.int_classification, 
                                                      prod_grp.prod_classification, ht_jur.jur_classification, ht_rlvnt.rlvnt_classification, bt_hint_date.date_classification,
                                                      bt_hint_url.url_classification, bt_hint_text.text_classification, bt_hint_discard_reason.discard_classification FROM 
                                                      (SELECT bt_hint_log.hint_id FROM bt_hint_log WHERE bt_hint_log.hint_id IN (%s)) change_ids 
                                                      LEFT JOIN (SELECT b221_hint_assessment.hint_id, b221_hint_assessment.assessment_accepted, b221_hint_assessment.validation_classification AS ass_classification FROM b221_hint_assessment JOIN (SELECT b221_hint_assessment.hint_id, MAX(b221_hint_assessment.validation_classification) AS newest_classification FROM b221_hint_assessment GROUP BY hint_id) newest_classification ON newest_classification.hint_id = b221_hint_assessment.hint_id AND newest_classification.newest_classification <=> b221_hint_assessment.validation_classification) ht_ass ON ht_ass.hint_id = change_ids.hint_id AND ht_ass.assessment_accepted = 1
                                                      LEFT JOIN (SELECT b221_hint_intervention.hint_id, b221_hint_intervention.intervention_accepted, b221_hint_intervention.validation_classification AS int_classification FROM b221_hint_intervention JOIN (SELECT b221_hint_intervention.hint_id, MAX(b221_hint_intervention.validation_classification) AS newest_classification FROM b221_hint_intervention GROUP BY hint_id) newest_classification ON newest_classification.hint_id = b221_hint_intervention.hint_id AND newest_classification.newest_classification <=> b221_hint_intervention.validation_classification) ht_int ON ht_int.hint_id = change_ids.hint_id AND ht_int.intervention_accepted = 1
                                                      LEFT JOIN (SELECT b221_hint_product_group.hint_id, b221_hint_product_group.product_group_assessment, b221_hint_product_group.validation_classification AS prod_classification FROM b221_hint_product_group JOIN (SELECT b221_hint_product_group.hint_id, MAX(b221_hint_product_group.validation_classification) AS newest_classification FROM b221_hint_product_group GROUP BY hint_id) newest_classification ON newest_classification.hint_id = b221_hint_product_group.hint_id AND newest_classification.newest_classification <=> b221_hint_product_group.validation_classification) prod_grp ON prod_grp.hint_id = change_ids.hint_id AND prod_grp.product_group_assessment = 1
                                                      LEFT JOIN (SELECT bt_hint_jurisdiction.hint_id, bt_hint_jurisdiction.jurisdiction_accepted, bt_hint_jurisdiction.validation_classification AS jur_classification FROM bt_hint_jurisdiction JOIN (SELECT bt_hint_jurisdiction.hint_id, MAX(bt_hint_jurisdiction.validation_classification) AS newest_classification FROM bt_hint_jurisdiction GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_jurisdiction.hint_id AND newest_classification.newest_classification <=> bt_hint_jurisdiction.validation_classification) ht_jur ON ht_jur.hint_id = change_ids.hint_id AND ht_jur.jurisdiction_accepted = 1
                                                      LEFT JOIN (SELECT bt_hint_relevance.hint_id, bt_hint_relevance.relevance_accepted, bt_hint_relevance.validation_classification AS rlvnt_classification FROM bt_hint_relevance JOIN (SELECT bt_hint_relevance.hint_id, MAX(bt_hint_relevance.validation_classification) AS newest_classification FROM bt_hint_relevance GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_relevance.hint_id AND newest_classification.newest_classification <=> bt_hint_relevance.validation_classification) ht_rlvnt ON ht_rlvnt.hint_id = change_ids.hint_id AND ht_rlvnt.relevance_accepted = 1
                                                      LEFT JOIN (SELECT bt_hint_date.hint_id, bt_hint_date.date_accepted, bt_hint_date.validation_classification AS date_classification FROM bt_hint_date JOIN (SELECT bt_hint_date.hint_id, MAX(bt_hint_date.validation_classification) AS newest_classification FROM bt_hint_date GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_date.hint_id AND newest_classification.newest_classification <=> bt_hint_date.validation_classification) bt_hint_date ON bt_hint_date.hint_id = change_ids.hint_id AND bt_hint_date.date_accepted = 1
                                                      LEFT JOIN (SELECT bt_hint_url.hint_id, bt_hint_url.url_accepted, bt_hint_url.validation_classification AS url_classification FROM bt_hint_url JOIN (SELECT bt_hint_url.hint_id, MAX(bt_hint_url.validation_classification ) AS newest_classification FROM bt_hint_url GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_url.hint_id AND newest_classification.newest_classification <=> bt_hint_url.validation_classification) bt_hint_url ON bt_hint_url.hint_id = change_ids.hint_id AND bt_hint_url.url_accepted = 1
                                                      LEFT JOIN (SELECT bt_hint_text.hint_id, bt_hint_text.description_accepted, bt_hint_text.validation_classification AS text_classification FROM bt_hint_text JOIN (SELECT bt_hint_text.hint_id, MAX(bt_hint_text.validation_classification ) AS newest_classification FROM bt_hint_text GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_text.hint_id AND newest_classification.newest_classification <=> bt_hint_text.validation_classification) bt_hint_text ON bt_hint_text.hint_id = change_ids.hint_id AND bt_hint_text.description_accepted = 1
                                                      LEFT JOIN (SELECT bt_hint_discard_reason.hint_id, bt_hint_discard_reason.reason_accepted, bt_hint_discard_reason.validation_classification AS discard_classification FROM bt_hint_discard_reason JOIN (SELECT bt_hint_discard_reason.hint_id, MAX(bt_hint_discard_reason.validation_classification ) AS newest_classification FROM bt_hint_discard_reason GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_discard_reason.hint_id AND newest_classification.newest_classification <=> bt_hint_discard_reason.validation_classification) bt_hint_discard_reason ON bt_hint_discard_reason.hint_id = change_ids.hint_id AND bt_hint_discard_reason.reason_accepted = 1;"),ifelse(paste0(confirmedHint, collapse = ',')=='',"NULL",paste0(confirmedHint, collapse = ',')))
    confirmedHint_classifications = gta_sql_get_value(confirmedHint_classifications)
    
    test_confirmedHint_classifications <<- confirmedHint_classifications

    dlvr_confirm_status(confirm.table=confirmedHint_classifications)
  })
  
  # DISCARD HINT ---------------------------------------------------------------
  observeEvent(input$discardHint, {
    discardedHint <<- jsonlite::fromJSON(input$discardHint)
    print(discardedHint)
    change.id = as.numeric(unique(discardedHint$hintId))
    
    b221_hint_change_attribute(change.id=change.id,
                               user.id = user$id,
                               is.superuser=T,
                               is.intervention=F,
                               intervention.modifiable=T,
                               add.discard.reason = discardedHint$reasons,
                               modify.discard.comment = discardedHint$comment,
                               modify.relevance = 0
                              )
    })

  observeEvent(input$duplicateRows, {
   print("Duplicate rows")
   duplicateData <- jsonlite::fromJSON(input$duplicateRows)
   starredHint <- as.numeric(unique(duplicateData$starred))
   duplicates <- as.numeric(unique(duplicateData$duplicate))
   
   # Check which hints are in collection
   collectionData <- gta_sql_multiple_queries(paste0("SELECT DISTINCT hint_id, collection_id FROM b221_hint_collection WHERE collection_id IN (SELECT DISTINCT collection_id FROM b221_hint_collection WHERE hint_id IN (",paste0(c(starredHint, duplicates), collapse=", "),"));"),output.queries = 1)
   in.collections <- switch(length(unique(collectionData$collection.id))>0, as.numeric(unique(collectionData$hint.id)), NULL)
   nr.collections <- switch(length(unique(collectionData$collection.id))>0, as.numeric(unique(collectionData$collection.id)), NULL)
   

   
   # no hint in collection -> create a new collection with these hints
   # multiple collections, but because starred hint not in collection -> create new collection
   if (is.null(in.collections) | (length(nr.collections)>1 & ! starredHint %in% in.collections) ) {
     
     # Get infos from starred hint to create collection attributes
     starred.info <- paste0("SELECT ht_log.hint_id, ht_log.hint_state_id, ht_log.acting_agency, ht_log.hint_date, jur_list.jurisdiction_name, jur_list.jurisdiction_id, ht_txt.hint_title, ht_txt.hint_description, ass_list.assessment_id, cltn_log.collection_id, cltn_log.collection_name,
                              GROUP_CONCAT(DISTINCT int_list.intervention_type_id SEPARATOR ' ; ')  AS intervention_type, 
                              GROUP_CONCAT(DISTINCT prod_list.product_group_id SEPARATOR ' ; ')  AS product_group_id,
                              GROUP_CONCAT(DISTINCT discard_list.discard_reason_id SEPARATOR ' ; ')  AS discard_reason_id,
                              GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='official', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS official,
                              GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='news', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS news,
                              GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='consultancy', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS consultancy,
                              GROUP_CONCAT(DISTINCT IF(bt_url_type_list.url_type_name='others', bt_url_log.url, NULL ) SEPARATOR ' ; ')  AS others,
                              (CASE WHEN ht_log.gta_id IS NOT NULL THEN 1 ELSE 0 END) AS is_intervention,
                              ht_act.state_act_id,
                              MAX(IF(bt_date_type_list.date_type_name='announcement', bt_hint_date.date, NULL )) AS announcement_date,
                              MAX(IF(bt_date_type_list.date_type_name='implementation', bt_hint_date.date, NULL )) AS implementation_date,
                              MAX(IF(bt_date_type_list.date_type_name='removal', bt_hint_date.date, NULL )) AS removal_date
                              FROM (SELECT ",starredHint," AS hint_id) my_hint
                              JOIN bt_hint_log ht_log ON my_hint.hint_id = ht_log.hint_id
                              LEFT JOIN (SELECT bt_hint_url.hint_id, bt_hint_url.url_id, bt_hint_url.url_type_id FROM bt_hint_url JOIN (SELECT bt_hint_url.hint_id, MAX(bt_hint_url.classification_id) AS newest_proposition, MAX(bt_hint_url.validation_classification) AS newest_validation FROM bt_hint_url WHERE hint_id = ",starredHint," GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_url.hint_id AND IF(newest_classification.newest_validation IS NOT NULL,newest_classification.newest_validation,newest_classification.newest_proposition) <=> IF(newest_classification.newest_validation IS NOT NULL,bt_hint_url.validation_classification,bt_hint_url.classification_id) AND (IF(newest_classification.newest_validation IS NOT NULL,bt_hint_url.url_accepted,1)=1 OR IF(newest_classification.newest_validation IS NOT NULL,bt_hint_url.url_accepted,NULL) IS NULL)) ht_url ON ht_url.hint_id = ht_log.hint_id JOIN bt_url_log ON ht_url.url_id = bt_url_log.url_id JOIN bt_url_type_list ON bt_url_type_list.url_type_id = ht_url.url_type_id
                              LEFT JOIN (SELECT bt_hint_jurisdiction.hint_id, bt_hint_jurisdiction.jurisdiction_id, bt_hint_jurisdiction.jurisdiction_accepted FROM bt_hint_jurisdiction JOIN (SELECT bt_hint_jurisdiction.hint_id, MAX(bt_hint_jurisdiction.classification_id) AS newest_proposition, MAX(bt_hint_jurisdiction.validation_classification) AS newest_validation FROM bt_hint_jurisdiction WHERE hint_id = ",starredHint," GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_jurisdiction.hint_id AND IF(newest_classification.newest_validation IS NOT NULL,newest_classification.newest_validation,newest_classification.newest_proposition) = IF(newest_classification.newest_validation IS NOT NULL,bt_hint_jurisdiction.validation_classification,bt_hint_jurisdiction.classification_id)) ht_jur ON ht_log.hint_id = ht_jur.hint_id AND (ht_jur.jurisdiction_accepted = 1 OR ht_jur.jurisdiction_accepted IS NULL) LEFT JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_id = ht_jur.jurisdiction_id
                              LEFT JOIN (SELECT bt_hint_text.hint_id, bt_hint_text.hint_description, bt_hint_text.description_accepted, bt_hint_text.hint_title FROM bt_hint_text JOIN (SELECT bt_hint_text.hint_id, MAX(bt_hint_text.classification_id) AS newest_proposition, MAX(bt_hint_text.validation_classification) AS newest_validation FROM bt_hint_text WHERE hint_id = ",starredHint," GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_text.hint_id AND IF(newest_classification.newest_validation IS NOT NULL,newest_classification.newest_validation,newest_classification.newest_proposition) = IF(newest_classification.newest_validation IS NOT NULL,bt_hint_text.validation_classification,bt_hint_text.classification_id)) ht_txt ON ht_txt.hint_id = ht_log.hint_id AND (ht_txt.description_accepted = 1 OR ht_txt.description_accepted IS NULL) 
                              LEFT JOIN (SELECT b221_hint_assessment.hint_id, b221_hint_assessment.assessment_id, b221_hint_assessment.assessment_accepted FROM b221_hint_assessment JOIN (SELECT b221_hint_assessment.hint_id, MAX(b221_hint_assessment.classification_id) AS newest_proposition, MAX(b221_hint_assessment.validation_classification) AS newest_validation FROM b221_hint_assessment WHERE hint_id = ",starredHint," GROUP BY hint_id) newest_classification ON newest_classification.hint_id = b221_hint_assessment.hint_id AND IF(newest_classification.newest_validation IS NOT NULL,newest_classification.newest_validation,newest_classification.newest_proposition) = IF(newest_classification.newest_validation IS NOT NULL,b221_hint_assessment.validation_classification,b221_hint_assessment.classification_id)) ht_ass ON ht_ass.hint_id = ht_log.hint_id AND (ht_ass.assessment_accepted = 1 OR ht_ass.assessment_accepted IS NULL) LEFT JOIN b221_assessment_list ass_list ON ass_list.assessment_id = ht_ass.assessment_id
                              LEFT JOIN (SELECT b221_hint_intervention.hint_id, b221_hint_intervention.apparent_intervention_id, b221_hint_intervention.intervention_accepted FROM b221_hint_intervention JOIN (SELECT b221_hint_intervention.hint_id, MAX(b221_hint_intervention.classification_id) AS newest_proposition, MAX(b221_hint_intervention.validation_classification) AS newest_validation FROM b221_hint_intervention WHERE hint_id = ",starredHint," GROUP BY hint_id) newest_classification ON newest_classification.hint_id = b221_hint_intervention.hint_id AND IF(newest_classification.newest_validation IS NOT NULL,newest_classification.newest_validation,newest_classification.newest_proposition) = IF(newest_classification.newest_validation IS NOT NULL,b221_hint_intervention.validation_classification,b221_hint_intervention.classification_id)) ht_int ON ht_int.hint_id = ht_log.hint_id AND (ht_int.intervention_accepted = 1 OR ht_int.intervention_accepted IS NULL) LEFT JOIN b221_intervention_type_list int_list ON int_list.intervention_type_id = ht_int.apparent_intervention_id
                              LEFT JOIN (SELECT b221_hint_product_group.hint_id, b221_hint_product_group.product_group_id, b221_hint_product_group.product_group_assessment FROM b221_hint_product_group JOIN (SELECT b221_hint_product_group.hint_id, MAX(b221_hint_product_group.classification_id) AS newest_proposition, MAX(b221_hint_product_group.validation_classification) AS newest_validation FROM b221_hint_product_group WHERE hint_id = ",starredHint," GROUP BY hint_id) newest_classification ON newest_classification.hint_id = b221_hint_product_group.hint_id AND IF(newest_classification.newest_validation IS NOT NULL,newest_classification.newest_validation,newest_classification.newest_proposition) = IF(newest_classification.newest_validation IS NOT NULL,b221_hint_product_group.validation_classification,b221_hint_product_group.classification_id)) ht_prod_grp ON ht_prod_grp.hint_id = ht_log.hint_id AND (ht_prod_grp.product_group_assessment = 1 OR ht_prod_grp.product_group_assessment IS NULL) LEFT JOIN b221_product_group_list prod_list ON prod_list.product_group_id = ht_prod_grp.product_group_id
                              LEFT JOIN (SELECT bt_hint_discard_reason.hint_id, bt_hint_discard_reason.discard_reason_id, bt_hint_discard_reason.reason_accepted FROM bt_hint_discard_reason JOIN (SELECT bt_hint_discard_reason.hint_id, MAX(bt_hint_discard_reason.classification_id) AS newest_proposition, MAX(bt_hint_discard_reason.validation_classification) AS newest_validation FROM bt_hint_discard_reason WHERE hint_id = ",starredHint," GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_discard_reason.hint_id AND IF(newest_classification.newest_validation IS NOT NULL,newest_classification.newest_validation,newest_classification.newest_proposition) = IF(newest_classification.newest_validation IS NOT NULL,bt_hint_discard_reason.validation_classification,bt_hint_discard_reason.classification_id)) ht_discard ON ht_discard.hint_id = ht_log.hint_id AND (ht_discard.reason_accepted = 1 OR ht_discard.reason_accepted IS NULL) LEFT JOIN bt_hint_discard_reason discard_list ON discard_list.discard_reason_id = ht_discard.discard_reason_id
                              LEFT JOIN (SELECT bt_hint_date.hint_id, bt_hint_date.`date`, bt_hint_date.date_type_id, bt_hint_date.date_accepted, bt_hint_date.confirm_status, bt_hint_date.validation_classification FROM bt_hint_date JOIN (SELECT bt_hint_date.hint_id, MAX(bt_hint_date.validation_classification) AS newest_classification FROM bt_hint_date GROUP BY hint_id) newest_classification ON newest_classification.hint_id = bt_hint_date.hint_id AND newest_classification.newest_classification <=> bt_hint_date.validation_classification) bt_hint_date ON bt_hint_date.hint_id = ht_log.hint_id AND (bt_hint_date.date_accepted = 1 OR bt_hint_date.date_accepted IS NULL) LEFT JOIN bt_date_type_list ON bt_hint_date.date_type_id = bt_date_type_list.date_type_id
                              LEFT JOIN b221_hint_collection ht_cltn ON ht_cltn.hint_id = ht_log.hint_id LEFT JOIN b221_collection_log cltn_log ON cltn_log.collection_id = ht_cltn.collection_id
                              LEFT JOIN bt_hint_state_act as ht_act ON ht_act.hint_id = ht_log.hint_id 
                              GROUP BY ht_log.hint_id;")
     
     if (is.null(in.collections)) {
     
       starred.info <- gta_sql_get_value(starred.info)
     colName <- paste0(starred.info$jurisdiction.name,": ",starred.info$hint.title," ", lubridate::month(starred.info$hint.date),"/",lubridate::year(starred.info$hint.date))
     attributes = bt_find_collection_attributes(new.collection.name = colName, 
                                                hints.id = c(starredHint, duplicates), 
                                                starred.hint.id = starredHints, 
                                                country = as.numeric(unlist(strsplit(na.omit(as.character(starred.info$jurisdiction.id)), " ; "))),
                                                product = as.numeric(unlist(strsplit(na.omit(as.character(starred.info$product.group.id)), " ; "))),
                                                intervention = as.numeric(unlist(strsplit(na.omit(as.character(starred.info$intervention.type)), " ; "))),
                                                assessment = as.numeric(unlist(strsplit(na.omit(as.character(starred.info$assessment.id)), " ; "))),
                                                relevance = 1, 
                                                discard = as.numeric(unlist(strsplit(na.omit(as.character(starred.info$discard.reason.id)), " ; "))),
                                                discard.comment = NULL, # still to do
                                                announcement.date = starred.info$announcement.date, 
                                                implementation.date = starred.info$implementation.date, 
                                                removal.date = starred.info$removal.date)
     
     hintIds <- c(starredHint, duplicates)
       
     } else {
       # FOURTH CASE, MORE THAN ONE COLLETIONS PRESENT, BUT NONE IS STARRED 
       starred.info <- gta_sql_get_value(starred.info)
       colName <- paste0(starred.info$jurisdiction.name,": ",starred.info$hint.title," ", lubridate::month(starred.info$hint.date),"/",lubridate::year(starred.info$hint.date))
       attributes = bt_find_collection_attributes(new.collection.name = colName, 
                                                  hints.id = c(starredHint, duplicates), 
                                                  starred.hint.id = starredHints, 
                                                  country = as.numeric(unlist(strsplit(na.omit(as.character(starred.info$jurisdiction.id)), " ; "))),
                                                  product = as.numeric(unlist(strsplit(na.omit(as.character(starred.info$product.group.id)), " ; "))),
                                                  intervention = as.numeric(unlist(strsplit(na.omit(as.character(starred.info$intervention.type)), " ; "))),
                                                  assessment = as.numeric(unlist(strsplit(na.omit(as.character(starred.info$assessment.id)), " ; "))),
                                                  relevance = 1, 
                                                  discard = as.numeric(unlist(strsplit(na.omit(as.character(starred.info$discard.reason.id)), " ; "))),
                                                  discard.comment = NULL, # still to do
                                                  announcement.date = starred.info$announcement.date, 
                                                  implementation.date = starred.info$implementation.date, 
                                                  removal.date = starred.info$removal.date)
       
       hintIds <- unique(c(starredHint, in.collections, duplicates))
       
       
       colName <- paste0(starred.info$jurisdiction.name,": ",starred.info$hint.title," ", lubridate::month(starred.info$hint.date),"/",lubridate::year(starred.info$hint.date))
     }
     
                                                     
     # if collection is existing
   } else {
     
     # Only one existing collection -> add remaining hints to that collection
       if (length(nr.collections)==1) {
       
        get.collection <- nr.collections
        hintIds <- unique(c(starredHint, in.collections, duplicates))
       
       # More than one collection -> Create new collection from all hints in all collections
     } else {
       
       # if starred hint in existing collection already -> move all hints from other collections to the starred one
         get.collection <- collectionData$collection.id[collectionData$hint.id == starredHint]
         hintIds <- unique(c(starredHint, in.collections, duplicates))
         
     }
     
     # get attributes of collection to be adjusted
     colattributes <- paste0("SELECT cltn_log.collection_id, cltn_log.collection_name, 
                              GROUP_CONCAT(DISTINCT(jur_list.jurisdiction_id) SEPARATOR ' ; ') AS jurisdiction_name,
                              GROUP_CONCAT(DISTINCT(ass_list.assessment_id) SEPARATOR ' ; ') AS assessment_name,
                              GROUP_CONCAT(DISTINCT(int_list.intervention_type_id) SEPARATOR ' ; ') AS intervention_type_name,
                              GROUP_CONCAT(DISTINCT(prod_grp_list.product_group_id) SEPARATOR ' ; ') AS product_group_name,
                              cltn_rel.relevance, cltn_star.hint_id AS starred_hint,
                              MAX(IF(bt_date_type_list.date_type_name='announcement', col_date.date, NULL )) AS announcement_date,
                              MAX(IF(bt_date_type_list.date_type_name='implementation', col_date.date, NULL )) AS implementation_date,
                              MAX(IF(bt_date_type_list.date_type_name='removal', col_date.date, NULL )) AS removal_date
                              FROM b221_collection_log cltn_log
                              JOIN b221_collection_jurisdiction cltn_jur ON cltn_jur.collection_id = cltn_log.collection_id AND cltn_log.collection_id = ",get.collection," JOIN gta_jurisdiction_list jur_list ON jur_list.jurisdiction_id = cltn_jur.jurisdiction_id
                              LEFT JOIN b221_collection_star cltn_star ON cltn_star.collection_id = cltn_log.collection_id
                              JOIN b221_collection_assessment cltn_ass ON cltn_ass.collection_id = cltn_log.collection_id JOIN b221_assessment_list ass_list ON cltn_ass.assessment_id = ass_list.assessment_id
                              JOIN b221_collection_intervention cltn_int ON cltn_int.collection_id = cltn_log.collection_id JOIN b221_intervention_type_list int_list ON int_list.intervention_type_id = cltn_int.intervention_type_id
                              JOIN b221_collection_product_group cltn_prod ON cltn_prod.collection_id = cltn_log.collection_id JOIN b221_product_group_list prod_grp_list ON prod_grp_list.product_group_id = cltn_prod.product_group_id
                              JOIN b221_collection_relevance cltn_rel ON cltn_rel.collection_id = cltn_log.collection_id
                              LEFT JOIN b221_collection_date col_date ON col_date.collection_id = cltn_log.collection_id LEFT JOIN bt_date_type_list ON col_date.date_type_id = bt_date_type_list.date_type_id
                              GROUP BY cltn_log.collection_id;")
     
     colattributes <- gta_sql_get_value(colattributes)
     
     attributes = bt_find_collection_attributes(collection.id = nr.collections, 
                                                hints.id = hintIds, 
                                                starred.hint.id = starredHint, 
                                                country = as.numeric(unlist(strsplit(na.omit(as.character(colattributes$jurisdiction.name)), " ; "))), 
                                                product = as.numeric(unlist(strsplit(na.omit(as.character(colattributes$product.group.name)), " ; "))), 
                                                intervention = as.numeric(unlist(strsplit(na.omit(as.character(colattributes$intervention.type.name)), " ; "))), 
                                                assessment = as.numeric(unlist(strsplit(na.omit(as.character(colattributes$assessment.name)), " ; "))), 
                                                relevance = 1, 
                                                discard = NULL, # still to do
                                                discard.comment = NULL, # still to do
                                                announcement.date = colattributes$announcement.date, 
                                                implementation.date = colattributes$implementation.date, 
                                                removal.date = colattributes$removal.date)
     
     colName <- colattributes$collection.name
     
   }
   
   collection.save =  b221_process_collections_hints(is.freelancer = F,
                                                     user.id = user$id,
                                                     new.collection.name = colName,
                                                     hints.id = hintIds,
                                                     starred.hint.id = attributes$starred.hint.id,
                                                     country = attributes$country,
                                                     product = attributes$product,
                                                     intervention = attributes$intervention,
                                                     assessment = attributes$assessment,
                                                     discard = attributes$discard,
                                                     discard.comment = attributes$discard.comment,
                                                     announcement.date = attributes$announcement.date,
                                                     implementation.date = attributes$implementation.date,
                                                     removal.date = attributes$removal.date,
                                                     relevance = 1,
                                                     collection.unchanged = F,
                                                     empty.attributes = F)
   
  })
  
}
