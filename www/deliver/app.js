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

const buttonsClicks = {
    accept: function(currentStatus, id) {
              if(['new', 'updated'].includes(currentStatus)){
                  $(`tr#${id}`).removeClass('new updated').addClass('confirmed').find('.status-label').text('confirmed');
                  $(`tr#${id}`).find('.accept').remove();
                  $('#DataTables_Table_0').DataTable().draw( false );
              } else {
                $('#DataTables_Table_0').DataTable().row(`tr#${id}`).remove().draw( false );
              }
            },
    delete: function(currentStatus, id) {
              let that = this;
              if(['new', 'updated', 'confirmed'].includes(currentStatus)){
                  $('#DataTables_Table_0').DataTable().row(`tr#${id}`).remove().draw( false );
              } else {
                  $(`tr#${id}`).removeClass('deleted').addClass('confirmed').find('.status-label').text('confirmed');
                  $(`tr#${id}`).find('.accept').remove();
                  $(`tr#${id}`).find('.delete').on('click', function(){ that.delete('confirmed', id) })
                  $('#DataTables_Table_0').DataTable().draw( false );
              }
            },
    edit: function() {},
    duplicate: function() {}
};