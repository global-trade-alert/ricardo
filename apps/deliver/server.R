# Define reactive values used in this app module up here, these will be passed automatically
# Don't forget to add these variables as function parameters as well

load(file.path("C:/Users/Liubomyr Gavryliv/Dropbox/ricardo-lg/apps/deliver/data/GTA-COVID data.Rdata"), new_env <- new.env() )

# SERVER
deliverserver <- function(input, output, session, user, app, prm, ...) {
  
  ns <- NS("deliver")
  
  output$deliverTable <- DT::renderDataTable(DT::datatable(
    data = new_env$covid.data, #retrieve_data(),
    rownames = FALSE,
    escape = FALSE,
    options = list(
      pageLength = 10,
      scrollX = TRUE,

      deferRender = TRUE,
      scrollY = JS('window.innerHeight - 160'),
      scroller = TRUE,
      
      #SearchPanes plugin
      dom = 'Pfrtip', 
      columnDefs = list(
          list(
            searchPanes = list(show = FALSE), 
            targets = 4:22
            ),
          list(
            searchPanes = list(
              options = list(
                list(
                label = 'food',
                value = DT::JS("function(rowData, rowIdx){\
                    return rowData[20] == true;\
                  }")
                ),
                list(
                  label = 'export barrier',
                  value = DT::JS("function(rowData, rowIdx){\
                    return rowData[11] == true;\
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
                )
              )
            ),
            targets = 1
          ),
          list(
            visible = FALSE,
            targets = 11:22
          )
        )
    ),
    # options = list(
    #   pageLength = 50,
    #   columnDefs = list(list(visible = FALSE, targets = c()), list(sortable=FALSE, targets = c(0)),
    #                     list(targets = c(), render = JS("
    #                                                     function(data, type, row, meta){
    #                                                       return '';
    #                                                     }
    #                                                   ")
    #                          )
    #                     ),
    #   order = list(),
    #   language = list(
    #     zeroRecords = "No more items available."
    #     ),
    #   rowCallback = JS(),
    #   initComplete = JS(),
    #   preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }') # reset
    #   # drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }') # bind select boxes to Shiny
    # ),
    # callback = JS(),
    extensions = c("Select", 'SearchPanes', 'FixedHeader'),
    selection = "none"
    ),
  server = F)
  
  
}