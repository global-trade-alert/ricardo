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
    
  let div = $('<div />').addClass('editMode');  
  let header = $('<h1 />').html('Edit Mode');
  let canvas = $('<div />').addClass('canvas');
  div.append(header, canvas);
    $('body').append(div);
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
        console.log(this.getRowData(id));
        let rowData = this.getRowData(id);
        rowData.forEach(function(d,i){
          
            let label = $("<label>").attr('for', `column-${d.index}`).html(`${d.name}`);
            let input;
            if (d.data !== null && d.data.length < 100){
                input = $('<textarea />')
                        .attr('id', `column-${d.index}`)
                        .attr('rows', 1)
                        .attr('cols', 30)
                        //.css({'resize': 'none'})
                        .val(`${d.data}`);
            } else {
                  input = $('<textarea />')
                        .attr('id', `column-${d.index}`)
                        .attr('rows', 10)
                        .attr('cols', 40)
                        .val(`${d.data}`);
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
      
      $('#save-edit').on('click', function(){
        let output= [];
          $('.canvas div textarea').each(function(){
              output.push($(this).val())
          });
          console.log(output)
          that.updateRowData(output, id);
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
    duplicate: function() {},
    convertToConfirmed: function(className, id){
      $(`tr#${id}`).removeClass(className).addClass('confirmed').find('.status-label').text('confirmed');
      $(`tr#${id}`).find('.accept').remove();
      $('#DataTables_Table_0').DataTable().row(`tr#${id}`).data()[0] = 'confirmed';
    },
    redrawDataTable: function(id){
      $('#DataTables_Table_0').DataTable().row(`tr#${id}`).invalidate().draw(false);
    },
    removeRow: function(id){
      //$(`tr#${id}`).fadeOut('fast', function(){
        $('#DataTables_Table_0').DataTable().row(`tr#${id}`).remove().draw( false );
      //});
    },
    updateSearchPanes: function(){
      $('#DataTables_Table_0').DataTable().settings()[0]._searchPanes.s.panes
                                            .filter(d => d.selections.length != 0).map(d => d.s.dt.draw(false)); //redraw searchPanes
    },
    updateRowData: function(data, id){
      $('#DataTables_Table_0').DataTable().row($(`tr#${id}`)).data(data);
      this.redrawDataTable();
      this.updateSearchPanes();
      this.rowAttachEvents(data[0], id);
    },
    getRowData: function(id){
        let columns = this.getColumnsNames();
        let output = [];
        $('#DataTables_Table_0').DataTable().row($(`tr#${id}`)).data().forEach(function(d,i){
          d = d == null ? '' : d.toString();
          output.push({data: d, name: columns[i].name, index: columns[i].index })
        })
        return output;
    },
    getColumnsNames: function(){
        let output = [];
        $('#DataTables_Table_0').DataTable().columns().every( function (i) {        
              //if (this.visible())
              output.push({ index: i, name: this.header().innerHTML})
        });
        return output;
    },
    rowAttachEvents: function(status, id){
        
        $(`tr#${id}`).find('.buttons-column').each(function(){
            let that = $(this);
            
              $(that).children().each(function(){
                $(this).on('click', function() { buttonsClicks[$(this).attr('class')](status, id) })
              })
        })
    }
};