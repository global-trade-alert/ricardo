// Global variables

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

$( document ).ready(function() {
    // add the overlay initially
  let overlay = $('<div />').addClass('overlay');
    $('body').hide().append(overlay).fadeIn(300);

    // add edit mode
  let div_edit = $('<div />').addClass('editMode');
  let headerWrap = $('<div />').addClass('editMode-header');
  let header = $('<h1 />').html('Edit Mode');
  let canvas = $('<div />').addClass('canvas');
  headerWrap.append(header);
  div_edit.append(headerWrap, canvas);
    $('body').append(div_edit);
// Append button to editMode
  $('.editMode .editMode-header').append(
    $('<button type="button" id="save-edit"><img src="www/deliver/save.svg" style="margin-right:10px;"/>Save data</button>')
    );

    // add html for discard prompt
      div_prompt = $('<div />')
                      .attr('id','prompt-form')
                      .attr('hidden', 'hidden')
                      .html(
                        '<p>Please, indicate a discard reason</p>\
                        <form>\
                          <fieldset>\
                           <label for="reason">Select reason</label>\
                            <select id="reason" class="discard">\
                              <option></option>\
                            </select>\
                           <label for="other">other</label>\
                            <textarea id="other"></textarea>\
                          </fieldset>\
                        </form>'
                      );

    $('body').append(div_prompt);

    (async() => {
      while(!window.hasOwnProperty("data_gta")) // wait till data_gta is loaded
          await new Promise(resolve => setTimeout(resolve, 1000));
       window.data_gta.discard_reason.map(function(d) {
                $('select#reason').append(
                  `<option value="${d}">${d}</option>`
                 )
                });

            $('select#reason').selectize({
              maxItems: 6,
              valueField: 'text',
              labelField: 'text',
              placeholder: "Choose reason...",
              create: false
            });
    })();



});

const buttonsClicks = {
    restore: function(currentStatus, id){
              const that = this;
              this.convertToConfirmed('deleted', id);
              $(`tr#${id}`).find('.restore').attr('style', 'display: none');
              $(`#toggle-description_${id}`).html() == 'Show less' ? $(`tr#${id}`).find('.more-less')[0].click() : '';
              this.redrawDataTable();
              this.updateSearchPanes();
    },
    accept: function(currentStatus, id) {
              this.convertToConfirmed('new updated', id);
              $(`#toggle-description_${id}`).html() == 'Show less' ? $(`tr#${id}`).find('.more-less')[0].click() : '';
              this.redrawDataTable();
              this.updateSearchPanes();
            },
    delete: function(currentStatus, id) {
              const that = this;
              if(['new', 'updated', 'confirmed'].includes(currentStatus)){
                  that.addDeletePrompt(currentStatus, id);
                  /*that.convertToDeleted(currentStatus, id);
                  $(`#toggle-description_${id}`).html() == 'Show less' ? $(`tr#${id}`).find('.more-less')[0].click() : '';*/
              } else {
                  this.removeRow(id);
              }
              this.redrawDataTable();
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
            switch (d.name.match(/date|description|source|product|instrument|jurisdiction|documentation status|assessment/gi)[0].toLowerCase()){
              case 'date':

                input = $('<input />')
                        .attr('type', 'text')
                        .attr('id', `column-${d.index}`)
                        .addClass('datepicker')
                        .attr('current-date', `${d.data.length == 0 ? '' : d.data}`);
                break;
              case 'description':
              case 'source':

                input = $('<textarea />')
                        .attr('id', `column-${d.index}`)
                        .attr('rows', 5)
                        .attr('cols', 40)
                        .val(`${d.data}`);
                break;
              case 'product':
              case 'instrument':
              case 'jurisdiction':

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
                break;
              case 'documentation status':

                let checked = /^official source/gi.test(d.data) ? true : false;
                input = $('<input />')
                        .attr('type', 'checkbox')
                        .attr('checked', checked)
                        .attr('id', `column-${d.index}`)
                        .addClass('doc-status');

                label = $("<label>").attr('for', `column-${d.index}`).html('Is official source?');
                break;
              case 'assessment':

                input = $('<select />')
                      .attr('id', `column-${d.index}`)
                      .addClass('assessment');

                window.data_gta[d.name].map(function(d1,i) {
                      let selected = d1 == d.data ? 'selected' : '';
                        input.append(
                          `<option value="${d1}" ${selected}>${d1}</option>`
                         )
                        })
                break;

            }


            if (input != undefined) {
              if (i <= 1) {
                $('.canvas').append(
                $('<div />').addClass(`inputs ${d.name.toLowerCase()}`)
                .append(label, input)
              )
              // display the first two normally, wrap the others in a div to display as grid
            } else {
                if (i == 2) {
                  $('.canvas').append(
                    $('<div />').addClass(`form-grid`)
                  )
                  }
                if (i >= 2) {
                  $('.canvas .form-grid').append(
                    $('<div />').addClass(`inputs ${d.name.toLowerCase()}`)
                    .append(label, input)
                  )
                }
            }
            }

            $('select.products').selectize({
              maxItems: 6,
              valueField: 'text',
              labelField: 'text',
              searchField: 'text',
              create: false
            });

            $('select.assessment').selectize({
              maxItems: 1,
              create: false
            });

        });

      $('.datepicker').bsDatepicker({ format: 'yyyy-mm-dd' });
      $('.datepicker').each(function(){
        if($(this).attr('current-date') != '')
          $(this).bsDatepicker('setDate', $(this).attr('current-date'))
      })

      $('#save-edit').on('click', function(){
        let output= [];
          $('.canvas div textarea,.datepicker,select.products,select.assessment').each(function(){
              let index = $(this).attr('id').match(/[0-9]+$/g)[0];
              let value = typeof($(this).val()) == 'string' ? $(this).val() : $(this).val().join(',');
              output.push({ data: value, index: parseInt(index) });
          });

          $('.doc-status').each(function(){ // separate for documentation status
              let index = $(this).attr('id').match(/[0-9]+$/g)[0];
              let value = $(this).is(':checked') ? 'Official source' : 'Non-official source';
              output.push({ data: value, index: parseInt(index) });
          });
          console.log(output)
          that.updateRowData(currentStatus, output, id);
          $('.overlay').click();
      })

      $('.overlay').addClass('show');
      $('.overlay').on('click', function(){
          $(this).removeClass('show');
          $( ".editMode" ).removeClass('show');
          $('.canvas').empty();
          $(this).unbind('click', arguments.callee);
      });

      $( ".editMode" ).addClass('show');

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
    convertToDeleted: function(className, id){
      $(`tr#${id}`).removeClass(className).addClass('deleted').find('.status-label').text('deleted');
      $(`tr#${id}`).find('.accept').remove();
      $(`tr#${id}`).find('.restore').attr('style', 'display: ');
      $('#DataTables_Table_0').DataTable().row(`tr#${id}`).data()[0] = 'deleted';
      this.rowAttachEvents('deleted', id);
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
                                'Removal date', 'Description', 'Source', 'Products', 'Instruments', 'Documentation status'];

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
    addDeletePrompt: function(currentStatus, id){
      const that = this;
          $(function() {
            $("#prompt-form").dialog({
                  autoOpen: false,
                  height: 300,
                  width: 250,
                  modal: true,
                  resizable: false,
                  buttons: {
                    OK: function() {
                      let selected = $('select#reason').selectize()[0].selectize.getValue(),
                          other = $('#prompt-form textarea').val(),
                          reasons = selected.concat(other).filter(d => d != '').join(',');

                      if (reasons.length == 0){
                        $('#other').addClass( "prompt-error" );
                        $('#prompt-form div.selectize-input').addClass( "prompt-error" );
                      } else {
                        that.convertToDeleted(currentStatus, id);
                        $(`#toggle-description_${id}`).html() == 'Show less' ? $(`tr#${id}`).find('.more-less')[0].click() : '';
                        $(this).dialog( "close" );
                        that.redrawDataTable();
                        that.updateSearchPanes();
                      }
                      console.log({selected: selected, other: other})
                    },
                    cancel: function(){
                      $(this).dialog( "close" );
                    }
                  },
                  open: function(){
                      $('select#reason').selectize()[0].selectize.clear();
                      $('.prompt-error').removeClass( "prompt-error" );
                      $('#prompt-form textarea').val('');
                  }
              });
          $("#prompt-form").dialog("open");
          //$("#prompt-form").parent().draggable( "disable" );
        });
    }
};

// On button click #search-pane-toggle-button collapse searchpanes
function searchPaneUI() {
  console.log('Toggling Class');
$('#deliver-deliverTable').on('click','#search-pane-toggle-button', function(){
  $('#deliver-deliverTable').toggleClass('collapsePanes');
});
}
