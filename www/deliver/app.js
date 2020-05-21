// FUNCTIONS

const toggleShowMore = function(Str, type, id){
  console.log(id)

  let output = `${Str}<a id=\'toggle-${type}_${row[0]}\' onclick=\'toggleShowLess(\"${Str}\",\"${type}\",${id});\' href=\'javascript:void(0);\'> Show less</a>`
  
    $(`#toggle-${type}_${id}`).parent('td').html(output)
}

const toggleShowLess = function(Str, type, id){
  console.log(id)
  
  return Str.substring(0, 100) + '...' + 
  `<a id=\'toggleButton\' onclick=\'toggleShowMore(\"${Str}\",\"${type}\",${id});\' href=\'javascript:void(0);\'> Show more</a>`
}

