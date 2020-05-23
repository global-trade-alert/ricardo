// FUNCTIONS

const showMore = function(type, id){

  $(`#toggle-${type}_${id}`).closest('td').find('.hidden-item').removeClass("hidden-item").addClass("unhidden");
  $(`#toggle-${type}_${id}`).closest('td').find('.trimmed-text').removeClass("trimmed-text").addClass("full-text");
  $(`#toggle-${type}_${id}`).html('...Show less');
  $(`#toggle-${type}_${id}`).attr('onclick', `showLess(\"${type}\",${id})`)
  
}

const showLess = function(type, id){

  $(`#toggle-${type}_${id}`).closest('td').find('.unhidden').removeClass( "unhidden" ).addClass( "hidden-item" );
  $(`#toggle-${type}_${id}`).closest('td').find('.full-text').removeClass("full-text").addClass("trimmed-text");
  $(`#toggle-${type}_${id}`).html('...Show more');
  $(`#toggle-${type}_${id}`).attr('onclick', `showMore(\"${type}\",${id})`)
  
}

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