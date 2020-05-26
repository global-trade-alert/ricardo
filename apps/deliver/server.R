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
  mutate(prd.other = ifelse(prd.other == TRUE, "product other", "")) %>%
  mutate(prd.food = ifelse(prd.food == TRUE, "food", "")) %>%
  mutate(prd.med.any = ifelse(prd.med.any == TRUE, "any medical product", "")) %>%
  mutate(products = paste(inst.export.barrier, inst.import.barrier, inst.domestic.subsidy,
                          inst.export.subsidy, inst.other, inst.unclear, prd.med.con, prd.med.eqm,
                          prd.med.drug, prd.other, prd.food, prd.med.any, inst.export.barrier, sep=',')) %>%
  select (-c(inst.export.barrier, inst.import.barrier, inst.domestic.subsidy,
             inst.export.subsidy, inst.other, inst.unclear, prd.med.con, prd.med.eqm,
             prd.med.drug, prd.other, prd.food, prd.med.any, inst.export.barrier))

new_env$covid.data$products <-
  new_env$covid.data$products %>%
    str_replace_all("\\,{2,}", ",") %>%
    str_replace_all("^\\,", "") %>%
    str_replace_all("\\,$", "")

# SERVER
deliverserver <- function(input, output, session, user, app, prm, ...) {
  
  ns <- NS("deliver")
  
  output$deliverTable <- DT::renderDataTable(DT::datatable(
    data = new_env$covid.data, #retrieve_data(),
    colnames = c('Documentation status' = 2, 'Jurisdiction' = 3, 'Initial assessment' = 4, 'GTA intervention type' = 5, 
                 'Announcement date' = 6, 'Implementation date' = 7, 'Removal date' = 8, 'Description' = 9,
                 'Source' = 10, 'Intervention Type' = 11, 'Instruments and Products' = 12),
    
    rownames = FALSE,
    escape = FALSE,
    options = list(
      rowId = JS("function(d) {
        return d[0];
      }"),
      pageLength = 30,
      scrollX = FALSE,

      deferRender = TRUE,
      scrollY = JS('window.innerHeight - 160'),
      scroller = TRUE,
      
      dom = 'Pfrtip', 
      
      searchPanes = list(
        cascadePanes = TRUE,
        viewTotal = TRUE
      ),
      
      language = list(
        searchPanes = list(
          count = '{total} found',
          countFiltered = '{shown} / {total}'
        )
      ),
      
      autoWidth = FALSE,
      columns = list(
           list(width = NULL), # Entry ID
           list(width = '5%'), # Documentation status
           list(width = '5%'), # Jurisdiction
           list(width = '5%'), # Initial assessment
           list(width = '5%'), # GTA intervention type
           list(width = '5%'), # Announcement date
           list(width = '5%'), # Implementation date
           list(width = '5%'), # Removal date 
           list(width = '60%'), # Description
           list(width = '5%'), # Source
           list(width = NULL), # Intervention type
           list(width = NULL) # Instruments and Products
      ),
      columnDefs = list(
          list(targets = '_all',
               createdCell = JS("function (td, cellData, rowData, row, col) {
                                  $(td).css('padding', '5px')
                                }
                                ")),
          list(targets = 11,
               render = JS("function (data, type, row) {
                            if (type === 'sp') {
                              return data.split(',')
                            }
                            return data;
                }"),
               searchPanes = list(
                 orthogonal = 'sp'
               )
               ),
          list(targets = 9,
               render = JS("function(data, type, row, meta){
                          data = data.replace(/(https?[^ ]+)/gi, '<a href=\"$1\" target=\"_blank\">$1</a>');

                          let output = `<div class=\"source-less\">${data}</div>`;

                          return output
               }")),
          list(targets = 8,
               render = JS("function(data, type, row, meta){
                          let output = `<div class=\"description-less\">${data}</div>`;

                          return output
               }")),
          
          # searchPanes extension
          list(
            searchPanes = list(
              show = FALSE
              ),
            targets = c(0:1,4:10)#,12:22
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
            targets = c(0,10,11) #11:22
          )
        ),
      
      initComplete = JS("function() {
                          
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

                              // add tr with product labels
                              console.log(api)

                              let data = api.rows( { page: 'current' } ).data();

                                console.log(data)
                                
                                for (let i in api.rows({ page: 'current' })[0]){
                                  let output = data[i][11].split(',').map(d => `<div class=\"item-label\">${d}</div>`).join('');
                                  
                                  api.$(`tr#${data[i][0]}`).after(`<tr id=\"labels_${data[i][0]}\" class=\"labels-tr\">
                                                                    <td colspan=\"9\">
                                                                      <div class=\"box-item-label\">${output}
                                                                      </div>
                                                                    </td>
                                                                  </tr>`);
                                }

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
      
      }"),
      
      createdRow = JS("function(row, data, dataIndex, cells){
      
      }")
    ),
    class = "hover stripe",
    extensions = c("Select", 'SearchPanes', 'FixedHeader'),
    selection = "none"
    ),
  server = F)
  
  
}