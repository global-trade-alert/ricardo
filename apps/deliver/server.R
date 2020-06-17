# Define reactive values used in this app module up here, these will be passed automatically
# Don't forget to add these variables as function parameters as well

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
  
observe({
  products_unique <<-
    gta_sql_get_value("SELECT product_group_name FROM b221_product_group_list")
  print(products_unique)
})

# SERVER
deliverserver <- function(input, output, session, user, app, prm, ...) {
  
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
        )
      ),
      autoWidth = FALSE,
      columnDefs = list(
       # set columns widths
          list(  # Confirmation status
            targets = 0,
            className = "dt-head-left status"
          ),
          list(  # Entry ID
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
          list( # Jurisdiction
            targets = 4,
            className = "dt-head-left smallPadding jurisdiction"
          ),
          list( # Initial assessment
            targets = 5,
            className = "dt-head-left smallPadding assessment"
          ),
          list( # GTA intervention type
            targets = 6,
            className = "dt-head-left smallPadding type"
          ),
          list( # Announcement date
            targets = 7,
            className = "dt-head-left smallPadding announcement-date"
          ),
          list(  # Implementation date
            targets = 8,
            className = "dt-head-left smallPadding implementation-date"
          ),
          list(  # Removal date
            targets = 9,
            className = "dt-head-left smallPadding removal-date"
          ), # 43%
          list(  # Description
            targets = 10,
            className = "dt-head-left smallPadding description"
          ),
          list(  # Source,
            targets = 11,
            className = "dt-head-left smallPadding source"
          ),
          list(  # Products
            targets = 12,
            className = "dt-head-left smallPadding products"
          ),
          list(  # Instruments
            targets = 13,
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
                            
                            let accepted = data != 'confirmed' ? '<img src=\"www/deliver/accept.png\" class=\"accept\" title=\"Confirm entry\"/>' : '',
                                deleted = '<img src=\"www/deliver/delete.png\" class=\"delete\" title=\"Remove entry\"/>',
                                edit = '<img src=\"www/deliver/edit.png\" class=\"edit\" title=\"Edit Entry\"/>',
                                duplicates = '<img src=\"www/deliver/duplicate.png\" class=\"duplicate\" title=\"Remove duplicates\"/>',
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
                                              <div class=\"status-column \">
                                                <div class=\"status-label ${data}\">${status}</div>
                                              </div>
                                              <div class=\"buttons-column\">${accepted + deleted + edit + duplicates + duplicates_remove}</div>
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
                          
                           // Add toggle button to bottom of search panes
                          console.log('TESTING ADD BUTTON FUNCTION');
                          let newNode = document.createElement('div');
                          newNode.setAttribute('id', 'search-pane-toggle-button');
                          let innerNode = document.createElement('span');
                          innerNode.setAttribute('class','material-icons');
                          innerNode.innerHTML = 'expand_less';
                          newNode.appendChild(innerNode);
                          console.log($('#deliver-deliverTable .dtsp-panesContainer'));
                          let referenceNode = $('#deliver-deliverTable .dtsp-panesContainer')[0];
                          referenceNode.parentNode.insertBefore(newNode, referenceNode.nextSibling);
                          
                          searchPaneUI();
                         
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
  
  
}