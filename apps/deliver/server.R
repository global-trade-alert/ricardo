# Define reactive values used in this app module up here, these will be passed automatically
# Don't forget to add these variables as function parameters as well

load(file.path("C:/Users/Liubomyr Gavryliv/Dropbox/ricardo-lg/apps/deliver/data/GTA-COVID data.Rdata"), new_env <- new.env() )

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
      pageLength = 10,
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
           list(width = '57%'), # Description
           list(width = '8%'), # Source
           list(width = NULL), # Intervention type
           list(width = '3%') # Instruments and Products
      ),
      columnDefs = list(
          list(targets = '_all',
               createdCell = JS("function (td, cellData, rowData, row, col) {
                                  $(td).css('padding', '5px')
                                }
                                ")),
          list(targets = 11,
                render = JS("function(data, type, row, meta){
                              let export_barrier = data != false ? `<div class=\'item-label\'>export barrier</div>` : null;
                              
                              
                              let import_barrier = row[12] != false ? `<div class=\'item-label\'>import barrier</div>` : null;
                              let dom_subsidy = row[13] != false ? `<div class=\'item-label\'>domestic subsidy</div>` : null;
                              let exp_subsidy = row[14] != false ? `<div class=\'item-label\'>export subsidy</div>` : null;
                              let other = row[15] != false ? `<div class=\'item-label\'>other</div>` : null;
                              let unclear = row[16] != false ? `<div class=\'item-label\'>unclear</div>` : null;
                              let med_con = row[17] != false ? `<div class=\'item-label\'>medical consumables</div>` : null;
                              let med_eqm = row[18] != false ? `<div class=\'item-label\'>medical equipment</div>` : null;
                              let med_drug = row[19] != false ? `<div class=\'item-label\'>medicines or drugs</div>` : null;
                              let food = row[20] != false ? `<div class=\'item-label\'>food</div>` : null;
                              let prd_other = row[21] != false ? `<div class=\'item-label\'>product other</div>` : null;
                              let med_any = row[22] != false ? `<div class=\'item-label\'>any medical product</div>` : null;
                              
                              return  '<div class=\"box-item-label\">' + [export_barrier, import_barrier, dom_subsidy,
                                       exp_subsidy, other, unclear, med_con, med_eqm, 
                                       med_drug, food, prd_other, med_any].filter(d => d != null).join('') + '</div>'
                            }")
               ),
          # add hyperlinks to sources and initialize ShowMore options
          # list(targets = 9,
          #      render = JS("function(data, type, row, meta){
          # 
          #                     let output = data.replace(/(https?[^ ]+)/gi, '<a href=\"$1\">$1</a>');
          # 
          #                     if (output != \"\" & output.length > 250){
          #                       let limit = '<div class=\"trimmed-text\">' + output.substr(0,200).lastIndexOf('<a') == -1 ? output.substr(0,200).lastIndexOf(' ') : output.substr(0,200).lastIndexOf('<a');
          #                       let trimmedString = output.substring(0, limit) + '<span class=\"hidden-item\">' +  output.substring(limit,output.length) + '</span>' +
          #                       `</div><a id=\'toggle-source_${row[0]}\' onclick=\'showMore(\"source\",${row[0]});\'  href=\"javascript:void(0);\">...Show more</a>`
          #                       return trimmedString;
          #                     } else {
          #                       return output;
          #                     }
          #                  }"
          #                )),
          # 
          # list(targets = 8,
          #      render = JS("function(data, type, row, meta){
          #                     if (data != \"\" & data.length > 250){
          #                       let limit = data.substr(0,200).lastIndexOf(' ');
          #                       let trimmedString = '<div class=\"trimmed-text\">' + data.substring(0, limit) + '<span class=\"hidden-item\">' +  data.substring(limit,data.length) + '</span>' +
          #                       `</div><a id=\'toggle-description_${row[0]}\' onclick=\'showMore(\"description\",${row[0]});\'  href=\"javascript:void(0);\">...Show more</a>`
          #                       return trimmedString;
          #                     } else {
          #                       return data;
          #                     }
          #      }")),
          list(targets = 9,
               render = JS("function(data, type, row, meta){
                          data = data.replace(/(https?[^ ]+)/gi, '<a href=\"$1\">$1</a>');

                          let output = `<div class=\"source-less\">${data}</div>
                          <button id =\"toggle-source_${row[0]}\" class=\"more-less\" onclick=\'showMorecontent(\"source\",${row[0]})\'>Show More</button>`;

                          return output
               }")),
          list(targets = 8,
               render = JS("function(data, type, row, meta){
                          let output = `<div class=\"description-less\">${data}</div>
                          <button id =\"toggle-description_${row[0]}\" class=\"more-less\" onclick=\'showMorecontent(\"description\",${row[0]})\'>Show More</button>`;

                          return output
               }")),
          # searchPanes extension
          list(
            searchPanes = list(show = FALSE),
            targets = c(0:1,4:10,12:22)#4:22
            ),
          
          list(
            searchPanes = list(
              header = 'Instruments and Products',
              options = list(
                list(
                  label = 'export barrier',
                  value = DT::JS("function(rowData, rowIdx){
                    return rowData[11] == true;
                  }")
                ),
                list(
                  label = 'import barrier',
                  value = DT::JS("function(rowData, rowIdx){\
                    return rowData[12] == true;\
                  }")
                ),
                list(
                  label = 'domestic subsidy',
                  value = DT::JS("function(rowData, rowIdx){\
                    return rowData[13] == true;\
                  }")
                ),
                list(
                  label = 'export subsidy',
                  value = DT::JS("function(rowData, rowIdx){\
                    return rowData[14] == true;\
                  }")
                ),
                list(
                  label = 'other',
                  value = DT::JS("function(rowData, rowIdx){\
                    return rowData[15] == true;\
                  }")
                ),
                list(
                  label = 'unclear',
                  value = DT::JS("function(rowData, rowIdx){\
                    return rowData[16] == true;\
                  }")
                ),
                list(
                  label = 'medical consumables',
                  value = DT::JS("function(rowData, rowIdx){\
                    return rowData[17] == true;\
                  }")
                ),
                list(
                  label = 'medical equipment',
                  value = DT::JS("function(rowData, rowIdx){\
                    return rowData[18] == true;\
                  }")
                ),
                list(
                  label = 'medicines or drugs',
                  value = DT::JS("function(rowData, rowIdx){\
                    return rowData[19] == true;\
                  }")
                ),
                list(
                  label = 'food',
                  value = DT::JS("function(rowData, rowIdx){\
                    return rowData[20] == true;\
                  }")
                ),
                list(
                  label = 'product other',
                  value = DT::JS("function(rowData, rowIdx){\
                    return rowData[21] == true;\
                  }")
                ),
                list(
                  label = 'any medical product',
                  value = DT::JS("function(rowData, rowIdx){\
                    return rowData[22] == true;\
                  }")
                )
              )
            ),
            targets = 11
          ),
          # Column visibility
          list(
            visible = FALSE,
            targets = c(0,10,11:22) #11
          )
        )
      # rowCallback = JS("function (row, data) {
      #                //top row
      #                let description = data[8] != null ? `<div id=\'description\'>${data[8]}</div>` : 'No description available';
      # 
      #                //middle row
      #                let entry_type = data[1] != null ? `<div id=\'entry-type\' class=\'item-label\'>${data[1]}</div>` : '';
      # 
      #                let country = data[2] != null ? `<div id= \'country\' class=\'item-label\'>${data[2]}</div>` : '';
      # 
      #                let initial_assessment = data[3] != null ? `<div id=\'initial-assessment\' class=\'item-label\'>${data[3]}</div>` : '';
      # 
      #                let intervention_type = data[4] != null ? `<div id=\'intervention-type\' class=\'item-label\'>${data[4]}</div>` : '';
      # 
      #                //bottom row
      #                let date_announced = data[5] != null ? `<div id=\'date-announced\' class=\'item-label\'>${data[5]}</div>` : '';
      # 
      #                let date_implemented = data[6] != null ? `<div id=\'date-implemented\' class=\'item-label\'>${data[6]}</div>` : '';
      # 
      #                let date_removed = data[7] != null ? `<div id=\'date-removed\' class=\'item-label\'>${data[7]}</div>` : '';
      # 
      #                let item = `<div class=\'top-item\'>${description}</div>
      #                            <div class=\'middle-item\'>${entry_type + country + initial_assessment + intervention_type}</div>
      #                            <div class=\'bottom-item\'>${date_announced + date_implemented + date_removed}</div>`;
      # 
      #                //$(row)
      #                //    .append(`<div id=${data[0]} class=\'item\'>${item}</div>`);
      # 
      #                //return row
      # }")
                          
    ),
    extensions = c("Select", 'SearchPanes', 'FixedHeader'),
    selection = "none"
    ),
  server = F)
  
  
}