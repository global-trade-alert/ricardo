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

new_env$covid.data <- 
  new_env$covid.data[c("confirmation","entry.id", "entry.type","country","initial.assessment",
                       "gta.intervention.type","date.announced","date.implemented","date.removed",
                       "description","source","intervention.type","products","instruments")]
  
# SERVER
deliverserver <- function(input, output, session, user, app, prm, ...) {
  
  ns <- NS("deliver")
  
  output$deliverTable <- DT::renderDataTable(DT::datatable(
    data = new_env$covid.data, #retrieve_data(),
    colnames = c('Confirmation Status' = 1, 'Documentation status' = 3, 'Jurisdiction' = 4, 'Initial assessment' = 5, 'GTA intervention type' = 6, 
                 'Announcement date' = 7, 'Implementation date' = 8, 'Removal date' = 9, 'Description' = 10,
                 'Source' = 11, 'Intervention Type' = 12, 'Products' = 13, 'Instruments' = 14),
    
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
          width = "4%",  # Confirmation status
          targets = 0
        ),
        list(
          width = "4%", # Documentation status
          targets = 2
        ),
        list(
          width = "4%", # Jurisdiction
          targets = 3
        ),
        list(
          width = "4%", # Initial assessment
          targets = 4
        ),
        list(
          width = "4%", # GTA intervention type
          targets = 5
        ),
        list(
          width = "4%", # Announcement date
          targets = 6
        ),
        list(
          width = "4%", # Implementation date
          targets = 7
        ),
        list(
          width = "4%", # Removal date
          targets = 8
        ),
        list(
          width = "42%", # Description
          targets = 9
        ),
        list(
          width = "20%", # Source
          targets = 10
        ),
        list(
          width = "7%", # Products
          targets = 12
        ),
        list(
          width = "7%", # Instruments
          targets = 13
        ),
          list(targets = '_all',
               createdCell = JS("function (td, cellData, rowData, row, col) {
                                  $(td).css('padding', '1px')
                                }
                                ")),
          list(targets = 0,
               render = JS("function (data, type, row){
                            if (type === 'sp') {
                              return data;
                            }
                           return `<div class=\"box-status-label\">
                                    <div class=\"status-label\">${data}</div>
                                   </div>`
               }"),
               searchPanes = list(
                 orthogonal = 'sp'
               )
               ),
          list(targets = 6,
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
                           console.log($('.dtsp-searchPane'))
                          $('.dtsp-searchPane:visible').removeClass('dtsp-columns-3');
                          $('.dtsp-searchPane:visible').addClass('dtsp-columns-5');
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
          list(targets = 10,
               render = JS("function(data, type, row, meta){
                          data = data.replace(/(https?[^ ]+)/gi, '<a href=\"$1\" target=\"_blank\">$1</a>');

                          let output = `<div class=\"source-less\">${data}</div>`;

                          return output
               }")),
          list(targets = 9,
               render = JS("function(data, type, row, meta){
                          let output = `<div class=\"description-less\">${data}</div>`;

                          return output
               }")),
          
          # searchPanes extension
          list(
            searchPanes = list(
              show = FALSE
              ),
            targets = c(1:2,5:11)#,12:22
            ),
          # list(
          #   searchPanes = list(
          #     header = 'Instruments and Products',
          #     combiner = 'and',
          #     options = list(
          #       list(
          #         label = 'export barrier',
          #         value = DT::JS("function(rowData, rowIdx){
          #           return /(?<![a-z]| )export barrier/g.test(rowData[11])
          #           //rowData[11].includes('export barrier')
          #         }")
          #       ),
          #       list(
          #         label = 'import barrier',
          #         value = DT::JS("function(rowData, rowIdx){\
          #           return /(?<![a-z]| )import barrier/g.test(rowData[11])
          #         }")
          #       ),
          #       list(
          #         label = 'domestic subsidy',
          #         value = DT::JS("function(rowData, rowIdx){\
          #           return /(?<![a-z]| )domestic subsidy/g.test(rowData[11])
          #         }")
          #       ),
          #       list(
          #         label = 'export subsidy',
          #         value = DT::JS("function(rowData, rowIdx){\
          #           return /(?<![a-z]| )export subsidy/g.test(rowData[11])
          #         }")
          #       ),
          #       list(
          #         label = 'other',
          #         value = DT::JS("function(rowData, rowIdx){\
          #           return /(?<![a-z]| )other/g.test(rowData[11])
          #         }")
          #       ),
          #       list(
          #         label = 'unclear',
          #         value = DT::JS("function(rowData, rowIdx){\
          #           return /(?<![a-z]| )unclear/g.test(rowData[11])
          #         }")
          #       ),
          #       list(
          #         label = 'medical consumables',
          #         value = DT::JS("function(rowData, rowIdx){\
          #           return /(?<![a-z]| )medical consumables/g.test(rowData[11])
          #         }")
          #       ),
          #       list(
          #         label = 'medical equipment',
          #         value = DT::JS("function(rowData, rowIdx){\
          #           return /(?<![a-z]| )medical equipment/g.test(rowData[11])
          #         }")
          #       ),
          #       list(
          #         label = 'medicines or drugs',
          #         value = DT::JS("function(rowData, rowIdx){\
          #           return /(?<![a-z]| )medicines or drugs/g.test(rowData[11])
          #         }")
          #       ),
          #       list(
          #         label = 'food',
          #         value = DT::JS("function(rowData, rowIdx){\
          #           return /(?<![a-z]| )food/g.test(rowData[11])
          #         }")
          #       ),
          #       list(
          #         label = 'product other',
          #         value = DT::JS("function(rowData, rowIdx){\
          #           return /(?<![a-z]| )product other/g.test(rowData[11])
          #         }")
          #       ),
          #       list(
          #         label = 'any medical product',
          #         value = DT::JS("function(rowData, rowIdx){\
          #           return /(?<![a-z]| )any medical product/g.test(rowData[11])
          #         }")
          #       )
          #     )
          #   ),
          #   targets = 11
          # ),
          # Column visibility
          list(
            visible = FALSE,
            targets = c(1,11) #11
          )
        ),
      
      initComplete = JS("function(settings) {

      }"),
      
      infoCallback = JS("function(settings, start, end, max, total, pre){
                                const api = this.api();

      }"),
      
      #clear display before creating content
      preDrawCallback = JS("function(settings){
                           const api = this.api();
                           api.$('.more-less').remove();

      }"),
      
      drawCallback = JS("function(settings){
                              const api = this.api();

                           
                               api.$('.status-label').each(function(){
                                  let type = $(this).text();
                                  $(this).closest('tr').addClass(type)
                                })

                              let data = api.rows( { page: 'current' } ).data();

                                console.log(data)
                                
                                /*for (let i in api.rows({ page: 'current' })[0]){
                                  let output = data[i][12].split(',').map(d => `<div class=\"item-label\">${d}</div>`).join('');
                                  
                                  api.$(`tr#${data[i][1]}`).after(`<tr id=\"labels_${data[i][1]}\" class=\"labels-tr\">
                                                                    <td colspan=\"9\">
                                                                      <div class=\"box-item-label\">${output}
                                                                      </div>
                                                                    </td>
                                                                  </tr>`);
                                }*/

                              // check if text length is bigger than tr height
                              function isEllipsisActive($jQueryObject) {
                                  return ($jQueryObject[0].offsetHeight < $jQueryObject[0].scrollHeight);
                              }

                              api.$('.description-less').each(function(){
                              let id = api.row( $(this).closest('tr') ).id();

                                  if(isEllipsisActive($(this)) == true){
                                      $(this).parent('td').append(`<button id =\"toggle-description_${id}\"
                                                                        class=\"more-less\" onclick=\'showMorecontent(\"description\",${id})\'>
                                                                        Show More</button>`)
                                  }
                              })
                        }"),
      
      rowCallback = JS("function(row, data){
                console.log(row)
                console.log(data)
      }"),
      
      createdRow = JS("function(row, data, dataIndex, cells){
        
      }")
    ),
    class = "row-border hover",
    extensions = c("Select", 'SearchPanes', 'FixedHeader'),
    selection = "none"
    ),
  server = F)
  
  
}