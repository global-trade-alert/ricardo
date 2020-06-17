# Define reactive values used in this app module up here, these will be passed automatically
# Don't forget to add these variables as function parameters as well
# SERVER

  load(file.path(paste0(path,"apps/deliver/data/GTA-COVID data.Rdata")), new_env <- new.env() )
  
  # preprocess data
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


deliverserver <- function(input, output, session, user, app, prm, ...) {
  
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
    
      session$sendCustomMessage('data_gta', shiny:::toJSON(list(Products = products_unique,
                                                                Instruments = instruments_unique,
                                                                Jurisdiction = jurisdiction_unique,
                                                                'Initial assessment' = assessment_unique,
                                                                discard_reason = discard_reason)))
  })
  
  ns <- NS("deliver")
  
  output$deliverTable <- DT::renderDataTable(DT::datatable(
    data = new_env$covid.data, #retrieve_data(),
    colnames = c('Confirmation Status' = 1, 'Entry ID' = 2, 'Users' = 3, 'Documentation status' = 4, 'Jurisdiction' = 5, 'Initial assessment' = 6, 'GTA intervention type' = 7, 
                 'Announcement date' = 8, 'Implementation date' = 9, 'Removal date' = 10, 'Description' = 11,
                 'Source' = 12, 'Products' = 13, 'Instruments' = 14),
    
    rownames = FALSE,
    escape = FALSE,
    options = list(
      rowId = JS("function(d) {
        return d[1];
      }"),
      pageLength = 30,
      scrollX = FALSE,

      deferRender = TRUE,
      scrollY = JS('window.innerHeight - 160'),
      scroller = TRUE,
      
      dom = 'Pfrtip', 
      
      searchPanes = list(
        cascadePanes = TRUE,
        viewTotal = TRUE,
        emptyMessage = "<i><b>no products</b></i>"
      ),
      
      language = list(
        searchPanes = list(
          count = '{total} found',
          countFiltered = '{shown} / {total}'
        )
      ),
      autoWidth = FALSE,
      columnDefs = list(
       # set columns widths
          list(
            width = "5%",  # Confirmation status
            targets = 0
          ),
          list(
            width = "3%",  # Entry ID
            targets = 1
          ),
          list(
            width = "3%",  # Users
            targets = 2
          ),
          list(
            width = "5%", # Documentation status
            targets = 3
          ),
          list(
            width = "5%", # Jurisdiction
            targets = 4
          ),
          list(
            width = "5%", # Initial assessment
            targets = 5
          ),
          list(
            width = "5%", # GTA intervention type
            targets = 6
          ),
          list(
            width = "4%", # Announcement date
            targets = 7
          ),
          list(
            width = "4%", # Implementation date
            targets = 8
          ),
          list(
            width = "4%", # Removal date
            targets = 9
          ), # 43%
          list(
            width = "24%", # Description
            targets = 10
          ),
          list(
            width = "13%", # Source
            targets = 11
          ),
          list(
            width = "10%", # Products
            targets = 12
          ),
          list(
            width = "10%", # Instruments
            targets = 13
          ),
          list(targets = '_all',
               createdCell = JS("function (td, cellData, rowData, row, col) {
                                  $(td).css('padding', '1px')
                                }
                                ")),
          list(targets = c(0:9),
               className = 'dt-center'),
          list(targets = 0,
               render = JS("function (data, type, row){
                            if (type === 'sp') {
                              return data;
                            }
                            
                            let accepted = !/confirmed|deleted/gi.test(data) ? '<img src=\"www/accept.png\" class=\"accept\" title=\"Confirm entry\"/>' : '',
                                deleted = '<img src=\"www/delete.svg\" class=\"delete\" title=\"Remove entry\"/>',
                                restore = /deleted/gi.test(data) ? '<img src=\"www/restore_page-black-18dp.svg\" class=\"restore\" title=\"Recover entry\"/>' : 
                                '<img src=\"www/restore_page-black-18dp.svg\" class=\"restore\" title=\"Recover entry\" style=\"display:none\"/>',
                                edit = '<img src=\"www/edit.png\" class=\"edit\" title=\"Edit Entry\"/>',
                                duplicates = '<img src=\"www/duplicate.png\" class=\"duplicate\" title=\"Remove duplicates\"/>',
                                duplicates_remove = '<input type=\"checkbox\" class=\"duplicates-remove\">';

                            
                            let output = `<div class=\"status-row\">
                                              <div class=\"buttons-column\">${accepted + restore + deleted + edit + duplicates + duplicates_remove}</div>
                                              <div class=\"status-column\">
                                                <div class=\"status-label\">${data}</div>
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
          list(targets = 4,
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
          list(targets = 7,
               render = JS("function(data,type,row){
                            if (data != null){
                                return `<div class=\"ann-date\">${data}</div>` 
                            } else {
                                return ''
                            }
               }")
               ),
          list(targets = 13,
               render = JS("function (data, type, row) {
                            if (type === 'sp') {
                              return data.split(',')
                            }
                            let output = data.split(',').map(d => `<div class=\"instr-label\">${d}</div>`);
                            
                            let left = [];
                            let right = [];
                            
                            for (let i in output){
                              if (output.length % 2 == 0) {
                                  i < output.length / 2 ? left.push(output[i]) : 
                                                right.push(output[i])
                              } else {
                                  i < Math.ceil(output.length / 2) ? left.push(output[i]) : 
                                                right.push(output[i])
                              }
                            }
                            
                            left = `<div class = \"col-left\">${left.join('')}</div>`;
                            right = `<div class = \"col-right\">${right.join('')}</div>`;
                            
                            return data != '' ? `<div class=\"box-item-label\">${left + right}</div>` : '';
                }"),
               searchPanes = list(
                 orthogonal = 'sp',
                 dtOpts = list(
                   initComplete = JS("function(){
                          $('.dtsp-searchPane:visible').removeClass('dtsp-columns-3');
                          $('.dtsp-searchPane:visible').addClass('dtsp-columns-5');
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
          list(targets = 12,
               render = JS("function (data, type, row) {
                            if (type === 'sp') {
                                return data.split(',')
                            } 
                            let output = data.split(',').map(d => `<div class=\"prd-label\">${d}</div>`)//.join('');
                            let left = [];
                            let right = [];
                            
                            for (let i in output){
                              if (output.length % 2 == 0) {
                                  i < output.length / 2 ? left.push(output[i]) : 
                                                right.push(output[i])
                              } else {
                                  i < Math.ceil(output.length / 2) ? left.push(output[i]) : 
                                                right.push(output[i])
                              }
                            }
                            
                            left = `<div class = \"col-left\">${left.join('')}</div>`;
                            right = `<div class = \"col-right\">${right.join('')}</div>`;
                            
                            return data != '' ? `<div class=\"box-item-label\">${left + right}</div>` : '';
                }"),
               searchPanes = list(
                 orthogonal = 'sp'
               )
               ),
          list(targets = 11,
               render = JS("function(data, type, row, meta){
                          data = data.replace(/(https?[^ ]+)/gi, '<a href=\"$1\" target=\"_blank\">$1</a>');

                          let output = `<div class=\"source-less\">${data}</div>`;

                          return output
               }")),
          list(targets = 10,
               render = JS("function(data, type, row, meta){
                          let output = `<div class=\"description-less\">${data}</div>`;

                          return output
               }")),
          
          # searchPanes extension
          list(
            searchPanes = list(
              show = FALSE
              ),
            targets = c(1:3,6:11)#,12:22
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
                                  let id = api.row( $(this).closest('tr') ).id();
                                
                                    if(isEllipsisActive($(this)) == true && $(this).siblings('.more-less').length == 0){
                                        $(this).parent('td').append(`<button id =\"toggle-description_${id}\"
                                                                          class=\"more-less\" onclick=\'showMorecontent(\"description\",${id})\'>
                                                                          Show More</button>`)
                                    }
                                })
                            })

      }"),
      
      drawCallback = JS("function(settings){
                              const api = this.api();
                              
                               api.$('.status-label').each(function(){
                                  let type = $(this).text();
                                  $(this).closest('tr').addClass(type)
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
  
}