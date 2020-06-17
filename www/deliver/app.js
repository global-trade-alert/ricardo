const showMorecontent = function(type, id){
  $(`#toggle-${type}_${id}`).closest('td').find(`.${type}-less`).removeClass(`${type}-less`).addClass(`${type}-more`);
  $(`#toggle-${type}_${id}`).addClass('open');
  $(`#toggle-${type}_${id}`).attr('onclick', `showLesscontent(\"${type}\",${id})`)
}

const showLesscontent = function(type, id){
  $(`#toggle-${type}_${id}`).closest('td').find(`.${type}-more`).removeClass(`${type}-more`).addClass(`${type}-less`);
  $(`#toggle-${type}_${id}`).removeClass('open');
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
        rowData.sort((a,b) => {
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
            } else {
            if (d.data !== null && d.data.length < 100){
                input = $('<textarea />')
                        .attr('id', `column-${d.index}`)
                        .attr('rows', 1)
                        .attr('cols', 30)
                        .val(`${d.data}`);
            } else {
                  input = $('<textarea />')
                        .attr('id', `column-${d.index}`)
                        .attr('rows', 10)
                        .attr('cols', 40)
                        .val(`${d.data}`);
            }
            }

            $('.canvas').append(
              $('<div />').append(
                          label, input
                      )
            )

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
          $('.canvas div textarea,.datepicker').each(function(){
              let index = $(this).attr('id').match(/[0-9]+$/g)[0];
              output.push({ data: $(this).val(), index: parseInt(index) });
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
            console.log('done')
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
      /*$('.duplicates-remove').each(function(){
        let id_this = $(this).closest('tr').attr('id');
        if( id_this != id);
          $(this).css({'display': 'block'});
          $(this).on('change', function(){
            if ($(this).is(':checked')) {
              that.addDuplicateOverlay(id_this);
            } else {
              that.removeDuplicateOverlay(id_this);
            }
        })
      });*/
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

// On button click #search-pane-toggle-button collapse searchpanes
function searchPaneUI() {
  console.log('Toggling Class');
$('#deliver-deliverTable').on('click','#search-pane-toggle-button', function(){
  $('#deliver-deliverTable').toggleClass('collapsePanes');
});
}
