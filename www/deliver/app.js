// Global variables
Shiny.addCustomMessageHandler('data_gta', function(data) {
   data.Instruments = data.Instruments.map(d => d.replace('domestic subsidy (incl. tax cuts, rescues etc.)', 'domestic subsidy'));
  window.data_gta = data;
});

const showMorecontent = function(type, id){
  $(`#toggle-${type}_${id}`).closest('td').find(`.${type}-less`).removeClass(`${type}-less`).addClass(`${type}-more`);
  $(`#toggle-${type}_${id}`).html('Show less');
  $(`#toggle-${type}_${id}`).attr('onclick', `showLesscontent(\"${type}\",${id})`)
}

const showLesscontent = function(type, id){
  $(`#toggle-${type}_${id}`).closest('td').find(`.${type}-more`).removeClass(`${type}-more`).addClass(`${type}-less`);
  $(`#toggle-${type}_${id}`).html('Show more');
  $(`#toggle-${type}_${id}`).attr('onclick', `showMorecontent(\"${type}\",${id})`)
}

// add the overlay initially
$( document ).ready(function() {
  let overlay = $('<div />').addClass('overlay');
    $('body').append(overlay);
    
    // add edit mode
  let div_edit = $('<div />').addClass('editMode');  
  let header = $('<h1 />').html('Edit Mode');
  let canvas = $('<div />').addClass('canvas');
  div_edit.append(header, canvas);
    $('body').append(div_edit);
});

const buttonsClicks = {
    accept: function(currentStatus, id) {
              if(['new', 'updated'].includes(currentStatus)){
                  this.convertToConfirmed('new updated', id);
                  $(`#toggle-description_${id}`).html() == 'Show less' ? $(`tr#${id}`).find('.more-less')[0].click() : '';
                  this.redrawDataTable();
              } else {
                  this.removeRow(id);
              }
              this.updateSearchPanes();
            },
    delete: function(currentStatus, id) {
              const that = this;
              if(['new', 'updated', 'confirmed'].includes(currentStatus)){
                  this.removeRow(id);
              } else {
                  this.convertToConfirmed('deleted', id);
                  $(`tr#${id}`).find('.delete').on('click', function(){ that.delete('confirmed', id) })
                  $(`#toggle-description_${id}`).html() == 'Show less' ? $(`tr#${id}`).find('.more-less')[0].click() : '';
                  this.redrawDataTable();
              }
              this.updateSearchPanes();
            },
    edit: function(currentStatus, id) {
        const that = this;
        let rowData = this.getRowData(id);
        console.log(rowData);
        rowData.sort((a,b) => { // custom sort to make Description and Source always be on top of .editMode 
            if (a.name == 'Description' | a.name== 'Source') {
                return -1
            } 
        });
        rowData.forEach(function(d,i){
          
            let label = $("<label>").attr('for', `column-${d.index}`).html(`${d.name}`);
            let input;
            if (/date/g.test(d.name)){
              input = $('<input />')
                        .attr('type', 'text')
                        .attr('id', `column-${d.index}`)
                        .addClass('datepicker')
                        .attr('current-date', `${d.data.length == 0 ? '' : d.data}`);
            } else if (/description|source/gi.test(d.name)) {
              input = $('<textarea />')
                        .attr('id', `column-${d.index}`)
                        .attr('rows', 10)
                        .attr('cols', 40)
                        .val(`${d.data}`);
            } else if (/product|instrument/gi.test(d.name)) {
              let data = d.data.split(',');
              console.log(data)
              input = $('<select />')
                      .attr('multiple', true)
                      .attr('id', `column-${d.index}`)
                      .addClass('products');
                      
                      
                window.data_gta[d.name].map(function(d1,i) {
                        let selected = data.includes(d1) ? 'selected' : '';
                        input.append(
                          `<option ${selected} value="${d1}">${d1}</option>`
                         )
                        })
                        
            }
                  
            $('.canvas').append(
              $('<div />').addClass('inputs')
              .append(label, input)
            )
            
            $('select.products').selectize({
              maxItems: 6,
              valueField: 'text',
              labelField: 'text',
              searchField: 'text',
              create: false
            });
            
        });
        
      $('.canvas').append(
        $('<input type="button" id="save-edit" value="Save data" />')
        );
      
      $('.datepicker').bsDatepicker({ format: 'yyyy-mm-dd' });
      $('.datepicker').each(function(){
        if($(this).attr('current-date') != '')
          $(this).bsDatepicker('setDate', $(this).attr('current-date'))
      })
       
      $('#save-edit').on('click', function(){
        let output= [];
          $('.canvas div textarea,.datepicker,select.products').each(function(){
              let index = $(this).attr('id').match(/[0-9]+$/g)[0];
              let value = typeof($(this).val()) == 'string' ? $(this).val() : $(this).val().join(',');
              output.push({ data: value, index: parseInt(index) });
          });
          console.log(output)
          that.updateRowData(currentStatus, output, id);
          $('.overlay').click();
      })
      
      $('.overlay').css({'display': 'block'});
                              
      $( ".editMode" ).animate({
          left: "+=540",
        }, 1000, function() {
          $('.overlay').on('click', function(){ 
              $(this).css({ 'display': 'none' });
              $( ".editMode" ).animate({ left: '-=540'}, 1000, function (){
                  $('.canvas').empty();
              });
              $(this).unbind('click', arguments.callee);
          });
      });
  
    },
    duplicate: function(status, id) {
      const that = this;
      let div_overlay = $('<div />')
                              .addClass('keep-row')
                              .css({ 'height': $(`tr#${id}`).height()});
      div_overlay.append($('<p />').text('Choose duplicates of this Entry'));
      let buttons = $('<div />').addClass('dupl-buttons')
                        .append($('<input id = "save-dupl" type="button" value="Remove duplicates"/>'))
                        .append($('<input id = "cancel-dupl" type="button" value="Cancel"/>'));
      
      div_overlay.append(buttons);
      
      $(`tr#${id}`).append(div_overlay);
      
      $('#save-dupl').on('click', function(){
        let rows = [];
        $('.remove-row').each(function() { //duplicates-remove:checked
          rows.push($(this).closest('tr').attr('id'));
      });
        that.stopDuplicatesMode();
        rows.forEach(d => that.removeRow(d));
      });
      $('#cancel-dupl').on('click', function(){
         that.stopDuplicatesMode();
      });
      
      $('.edit,.duplicate,.delete,.accept').each(function(){ $(this).css({'display': 'none'}) });
      $('#DataTables_Table_0 tr').each(function(){
          const this_row = $(this);
          let id_this = this_row.attr('id');
          if( id_this != id){
            $(this).on('click', function(){
                that.addDuplicateOverlay(id_this);
                $(this).unbind('click', arguments.callee);
            })
          }
      })
    },
    convertToConfirmed: function(className, id){
      $(`tr#${id}`).removeClass(className).addClass('confirmed').find('.status-label').text('confirmed');
      $(`tr#${id}`).find('.accept').remove();
      $('#DataTables_Table_0').DataTable().row(`tr#${id}`).data()[0] = 'confirmed';
      this.rowAttachEvents('confirmed', id);
    },
    redrawDataTable: function(id){
      $('#DataTables_Table_0').DataTable().row(`tr#${id}`).invalidate().draw(false);
    },
    removeRow: function(id){
      //$(`tr#${id}`).fadeOut('fast', function(){
        if (id != undefined);
        $('#DataTables_Table_0').DataTable().row(`tr#${id}`).remove().draw(false);
      //});
    },
    updateSearchPanes: function(){
      $('#DataTables_Table_0').DataTable().settings()[0]._searchPanes.s.panes
                                            .filter(d => d.selections.length != 0).map(d => d.s.dt.draw(false)); //redraw searchPanes
    },
    updateRowData: function(currentStatus, data, id){
      console.log(currentStatus)
      $(`tr#${id}`).removeClass(currentStatus);
      data.map(function(d){
          $('#DataTables_Table_0').DataTable().cell($(`tr#${id}`), d.index).data(d.data)
      })
      this.redrawDataTable();
      this.updateSearchPanes();
      this.rowAttachEvents(currentStatus, id);
    },
    getRowData: function(id){
        let columns = this.getColumnsNames();
        let col_ind = columns.flatMap(d => d.index);
        console.log(columns)
        let output = [];
        $('#DataTables_Table_0').DataTable().row($(`tr#${id}`)).data().forEach(function(d,i){
          d = d == null ? '' : d.toString();
          if (col_ind.indexOf(i) != -1)
          output.push({data: d, name: columns.filter(d => d.index == i)[0].name, index: columns.filter(d => d.index == i)[0].index })
        })
        return output;
    },
    getColumnsNames: function(){
        let output = [];
        let filtered_columns = ['Jurisdiction', 'Initial assessment', 'Announcement date', 'Implementation date', 
                                'Removal date', 'Description', 'Source', 'Products', 'Instruments'];
                                
        $('#DataTables_Table_0').DataTable().columns().every( function (i) {        
              if (filtered_columns.includes(this.header().innerHTML))
              output.push({ index: i, name: this.header().innerHTML})
        });
        return output;
    },
    rowAttachEvents: function(status, id){
        $(`tr#${id}`).find('.buttons-column').each(function(){
            let that = $(this);
            
              $(that).children().each(function(){
                if ($(this).attr('class') != 'duplicates-remove')
                $(this).off('click').on('click', function() { buttonsClicks[$(this).attr('class')](status, id) })
              })
        })
    },
    stopDuplicatesMode: function(){
      $('.keep-row').remove();
      $('.remove-row').remove();
      $('.edit,.duplicate,.delete,.accept').each(function(){ $(this).css({'display': ''}) });
      /*$('.duplicates-remove').each(function(){ 
        $(this).off('change');
        $(this).prop( "checked", false ).css({'display': 'none'});
      });*/
       $('#DataTables_Table_0 tr').each(function(){
         $(this).off('click');
      })
    },
    addDuplicateOverlay: function(id){
      const that = this;
      let div_overlay = $('<div />')
                              .addClass('remove-row')
                              .css({ 'height': $(`tr#${id}`).height()})
                              
      div_overlay.append($('<p />').text('Duplicate row'));
      
      $(`tr#${id}`).append(div_overlay);
      $(`tr#${id}`).find('.remove-row').on('click', function(){ 
          event.stopPropagation();
          $(`tr#${id}`).on('click', function(){
                const this_inner = $(this);
                that.addDuplicateOverlay(id);
                this_inner.unbind('click', arguments.callee);
            });
          $(this).remove();
      });
    },
    removeDuplicateOverlay: function(id){
      
    }
};