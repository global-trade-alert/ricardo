// FUNCTIONS

// wait till content is added to HTML
  (async() => {
    while(document.querySelectorAll('.control-bar,#b221-slideInRight,#confirm-discard').length == 0) 
    await new Promise(resolve => setTimeout(resolve, 1000));
    console.log('content is loaded')
    $('.control-bar').css({'visibility': ''});
    $('#b221-slideInRight').css({'visibility': ''});
    $('#confirm-discard').css({'visibility': ''});
    discardButton();
  })();

// REACTIVATING LEADS ITEM
function checkLeads() {
  $('#b221-leadsTable').on('click', '.leads-item .right-col .evaluate',function (e) {
   // Check if sender is the <div> element e.g.
   // console.log('checkLeads');
    var elementType = $(this)[0].id

    var elementID = $(this.parentNode.parentNode.parentNode)[0].id

    var changeEl = $(this.parentNode.parentNode.parentNode)

    if(!$(e.target).is('#b221-leadsTable .no-touch')) {
      if (elementType == "discard" && ! $(this).hasClass('dismiss')) {
          $(changeEl).removeClass('reactivate');
          $(changeEl).addClass('dismiss');
          // Shiny.setInputValue("b221-checkLeadsClick", [elementType, elementID], {priority: "event"});
          // collectData(`#${elementID}`,'dismiss');
      } else if (elementType == "relevant" && ! $(this).hasClass('reactivate')) {
          $(changeEl).removeClass('dismiss');
          // console.log(`#${elementID}`);
          // collectData(`#${elementID}`,'reactivate');
      }
  }

  });
}

// REACTIVATING LEADS ITEM
function checkLeadsManual() {
  $('#b221-leadsTable').on('click', '.leads-item .right-col .evaluate',function (e) {
   // Check if sender is the <div> element e.g.
   // console.log('checkLeadsManual');
    var elementType = $(this)[0].id

    var elementID = $(this.parentNode.parentNode.parentNode)[0].id

    var changeEl = $(this.parentNode.parentNode.parentNode)

    if(!$(e.target).is('#b221-leadsTable .no-touch')) {
      if (elementType == "discard" && ! $(this).hasClass('dismiss')) {
          $(changeEl).removeClass('reactivate');
          $(changeEl).addClass('dismiss');

          // Shiny.setInputValue("b221-checkLeadsClick", [elementType, elementID], {priority: "event"});
          //collectData(`#${elementID}`,'dismiss');
          $('#confirm-discard > div > div.form-group').css({'display': ''})
          del_or_dis();
          $('#confirm-discard').addClass(`show ${elementID}`);
          
      } else if (elementType == "relevant" && ! $(this).hasClass('reactivate')) {
          $(changeEl).removeClass('dismiss');
          // console.log(`#${elementID}`);
          collectData(`#${elementID}`,'reactivate');
      }
  }

  });
}

function hintsBasicUI() {

  console.log("HINTS BASIC UI BOUND");

  // SHOW MORE BUTTON
  $('#b221-leadsTable').on('click', '.leads-item .middle-row .translation-toggle',function () {
    event.stopPropagation(); // prevent bubbling to .leads-item
    $(this.parentNode.parentNode).toggleClass('translate');
  });

  // SHOW MORE BUTTON
  $('#b221-leadsTable').on('click', '.leads-item .middle-row .show-more',function () {
    $(this.parentNode.parentNode.parentNode.parentNode).toggleClass('showAll');
  });

  $('#b221-leadsTable').on('click', '.leads-item .collection-add',function () {
    // console.log("1 pressing collection-add button");
    var elementID = $(this.parentNode.parentNode.parentNode)[0].id
    if ($(this).hasClass('partOfCollection')){
      var collectionID = $(this)[0].id;
      var collection = "TRUE";
    } else if ($(this).hasClass('noPartOfCollection')) {
      var collectionID = "FALSE";
      var collection = "FALSE";
    }
    // console.log([elementID, collectionID, collection]);

    Shiny.setInputValue("b221-collectionAdd", [elementID, collectionID, collection], {priority: "event"});
    Shiny.setInputValue("b221-loadCollections", elementID.replace("leadsID_",""), {priority: "event"});
    Shiny.setInputValue("b221-loadSingleHints", elementID, {priority: "event"});

  });


  $('#b221-slideInRight').on('loadCollectionSlideIn',function () {
    var elementID = $(this)[0].children[0].id
    // console.log("LOAD COLLECTIONS SLIDE IN")
    // console.log(elementID);
    $('#b221-close-button').addClass('open');
    $('.backdrop-nav').addClass('open');
  });

  // SHOW COUNTRIES DROPDOWNS ON SELECT
  $('#b221-leadsTable').on('click','.top-row',function (event) {
    event.stopPropagation(); // prevent bubbling to .leads-item
  });


  $('#b221-leadsTable').on('click','.submission',function (event) {
    event.stopPropagation(); // prevent bubbling to .leads-item
  });

  $('#b221-leadsTable').on('change','.selectize-input input',function (event) {
    $(this.parentNode.parentNode.parentNode.parentNode.parentNode.parentNode).addClass('show-submission');
  });

  $('#b221-leadsTable').on('change','.is-official input',function (event) {
    $(this.parentNode.parentNode.parentNode.parentNode.parentNode).addClass('show-submission');
  });

  $('#b221-leadsTable').on('change','.top-row select',function (event) {
    $(this.parentNode.parentNode.parentNode.parentNode.parentNode).addClass('show-submission');
  });

  $('#b221-leadsTable').on('change keyup paste','.comment input',function (event) {
    $(this.parentNode.parentNode.parentNode.parentNode).addClass('show-submission');
  });

  $('#b221-leadsTable').on('mouseenter', '.leads-item', function(event){
    // console.log("MOUSENTER SUCCESS 3");
    // gather all select box classes => pass to object => pass to server
    let country_select = $('[id^=country]', this);
    let current_country = $(country_select).val();
    let elementID =  $(country_select).attr('id').replace('country_','');
  })

}

function slideInBasicUI() {

  $('.backdrop-nav').on('click', function () {
    $('#b221-slideInRight').removeClass('open');
    $('#b221-close-button').removeClass('open');
  });

  $('#b221-close-button').on('click', function () {
    $('#b221-slideInRight').removeClass('open');
    $('#b221-close-button').removeClass('open');
    $('.backdrop-nav').removeClass('open');
  });

  $('#b221-slideInRight').on('input','.textAreaCollection', function () {
    var colID = $(this)[0].id.replace("textCollection_","");
    $(`#renameCollection_${colID}`).addClass('visible');
    console.log($(this));
  });

  $('#b221-slideInRight').on('click', '.addCollection', function () {
    var colID = $(this)[0].id.replace("addCollection_","")
    Shiny.setInputValue("b221-collectionClick", colID, {priority: "event"});
  });

  $('#b221-slideInRight').on('click', '.renameCollection', function () {
    var colID = $(this)[0].id.replace("renameCollection_","")
    var name = $(`#textCollection_${colID}`)[0].value;
    output = ({id: colID, name: name});
    Shiny.setInputValue("b221-renameCollection", JSON.stringify(output), {priority: "event"});
    $(`#renameCollection_${colID}`).removeClass('visible');
  });

}

function slideInDiscardButton() {
  $('#b221-slideInRight').on('click','#discardCollection-popup', function () {
    del_or_dis('Discard','collection','Mark irrelevant');
    $('#confirm-discard > div > div.form-group').css({'display': ''})
    $('#confirm-discard').addClass('show');
    $('#b221-discardSelect').selectize()[0].selectize.clear(); //clear discard-select
    $('#b221-discardOther').val(''); //clear discard-other
  });
    $('#b221-slideInRight').on('click','#deleteCollection-popup', function () {
    $('#confirm-discard > div > div.discard-select-fields').css({'display': 'none'})
    del_or_dis('Delete', 'collection', 'Delete');
    $('#confirm-discard').addClass('show');
  })
}

function discardButton() {

// CANCEL BUTTON
  $('#confirm-discard .cancel').on('click', function () {
    $('#confirm-discard').removeClass('show');
    $('#b221-discardSelect').selectize()[0].selectize.clear(); //clear discard-select
    $('#b221-discardOther').val(''); //clear discard-other
  let id = $('#confirm-discard').attr('class').match(/leadsID_.*/gi); //checks if pop-up was initiated from .evaluate button
  if (id != null){
    $(`#${id[0]}`).removeClass('dismiss');
  }

  $('#confirm-discard').removeClass (function (index, className) {
      return (className.match (/leadsID_.*/gi) || []).join(' ');
  });
})

// DELETE COLLECTION BUTTON
  $('#confirm-discard #b221-discardCollection').on('click', function () {

  })

}


function markHints() {

  $('#b221-slideInRight').on('click','#hintContainer .hint-title', function (e) {
    console.log(e);
    if(e.target.className == 'hint-title') {
    if ($(this.parentNode.parentNode).hasClass('starred')) {
      $(this.parentNode.parentNode).removeClass('starred');
    } else {
      $('#b221-slideInRight #hintContainer .hint-item').removeClass('starred');
      var starred = $(this.parentNode.parentNode)[0].id.replace("hintId_","");
      $(this.parentNode.parentNode).addClass('starred');
      Shiny.setInputValue("b221-newStarred", starred, {priority: "event"});
    }
    }
})

// SHOW COUNTRIES DROPDOWNS ON SELECT
$('#b221-singleHintsTable .right .info').on('mouseenter','.top-row',function (event) {
  // console.log("STOPPING BACK PROPAGATION");
  event.stopPropagation(); // prevent bubbling to .leads-item
});

}


function removeHint() {
  // console.log('REMOVE HINT CALLED');
  $('#b221-slideInRight').on('click','.remove',function (event) {
    // console.log($(this.parentNode.parentNode));
    event.stopPropagation();
    var removeElement = $(this.parentNode.parentNode);
    var removeId = $(this.parentNode.parentNode)[0].id.replace("hintId_","");
    removeElement.fadeOut(300, function(){
      $(this).remove();
      Shiny.setInputValue("b221-removeHint", removeId, {priority: "event"});
    });
  });

  $('#b221-slideInRight').on('click', '.is-official',function () {
    // console.log($(this.parentNode.parentNode).hasClass('official'));
    if ($(this.parentNode.parentNode).hasClass('official')) {
      $(this.parentNode.parentNode).removeClass('official');
    } else {
      $(this.parentNode.parentNode).addClass('official');
    }
  });
}

// EVENT LISTENER FOR LEADS_ITEM ARRIVING AT TOP
function leadsDismiss() {
  // console.log("LEADS DISMISSED FUNCTION LOADED");
    $('.leads-item').each(function () {
        var $this = $(this);
        var offset = $this.offset().top;
        var scrollTop = $(window).scrollTop();
        var elementID = $(this)[0].id
        var elementType = "relevant"
        // console.log('Scrolltop' + scrollTop);
        // console.log('offset' + offset);
        if (scrollTop > offset && !$(this).hasClass('checked')) {
          // Shiny.setInputValue("b221-checkLeads", elementID, {priority: "event"});
          $(this).addClass('checked');
          // console.log("LEAD CHECKED");
          if (!$(this).hasClass('dismiss') ) {
            // Shiny.setInputValue("b221-checkLeadsClick", [elementType, elementID], {priority: "event"});
            // console.log("LEAD CHECKED");
            collectData(`#${elementID}`, 'reactivate');
          }
        }
    });
}

function callLeadsDismiss() {
  // console.log('LEADS DISMISSED CALLED');
  $(window).scroll(leadsDismiss);
}

// Submit data of one hint
function submitSingleHint() {
  // console.log("submitSingleHint");
  $('#b221-leadsTable').on('click', '.leads-item [id^=submit]',function () {
     // console.log(this)
     let state = $(this.parentNode.parentNode.parentNode).hasClass('dismiss') ? "dismiss" : "reactivate";
     // console.log(state);
     let id = $(this)[0].id.replace("submit_", "");
     collectData(`#leadsID_${id}`, state);
  });
}

function checkNull(el) {
  return el == null;
}

//  Collect data for a general submit of hints
function collectData(type='', state=''){
  console.log("Collector running");
  let selector = `${type}`;
  let output = [];
  // console.log(state);
  // console.log(state=="reactivate");
  try {
      $(selector).each(function () {
          let clicked = state == 'reactivate' ? 1 : 0 // add logics if hint is not selected at all
          let id = $(this)[0].id.replace("leadsID_", "");
          let country = $(`#country_${id}`).val() != null ?  $(`#country_${id}`).val().join(' ; ') : [null]
          let product = $(`#product_${id}`).val() != null ? $(`#product_${id}`).val().join(' ; ') : [null]
          let intervention = $(`#intervention_${id}`).val() != null ? $(`#intervention_${id}`).val().join(' ; ') : [null]
          let assessment = $(`#assessment_${id}`).val();
          let comment = $(`#comment_${id}`).val().length != 0 ? $(`#comment_${id}`).val() : null;
          let url = $(`#leadsID_${id} .background-url a`).attr('href');
          let official = $(`#official_${id}`).is(':checked') ? 1 : 0;
          let announcementdate = $(`#announcementdate_${id} input`).val().length != 0 ? $(`#announcementdate_${id} input`).val() : null;
          let implementationdate = $(`#implementationdate_${id} input`).val().length != 0 ? $(`#implementationdate_${id} input`).val() : null;
          let removaldate = $(`#removaldate_${id} input`).val().length != 0 ? $(`#removaldate_${id} input`).val() : null;
          let discard_reasons_select = $('#b221-discardSelect').val().length == 0 ? [null] : $('#b221-discardSelect').val().join(' ; ');
          let discard_comment = $('#b221-discardOther').val() == "" ? null : $('#b221-discardOther').val();
          output.push({id: id, clicked: clicked, country: country, product: product, intervention: intervention,
                      assessment: assessment, official: official, comment: comment, url: url, announcementdate: announcementdate,                             implementationdate: implementationdate, removaldate: removaldate, discard_reasons: discard_reasons_select,
                      discard_comment: discard_comment
          })
    });
     console.log(output)
    let validate = [output[0].country[0], output[0].product[0], output[0].intervention[0]];

    if (state == 'reactivate') {
        if (validate.some(checkNull)) {
          Shiny.setInputValue("b221-showError", "allFields", {priority: "event"});
        } else {
          Shiny.setInputValue("b221-collectedData", JSON.stringify(output), {priority: "event"});
          $(`${type}`).addClass('reactivate');
          $(`${type}`).removeClass('show-submission')
        }
      } else {
        Shiny.setInputValue("b221-collectedData", JSON.stringify(output), {priority: "event"});
        $(`${type}`).addClass('dismiss');
        $(`${type}`).removeClass('show-submission')
    }
  }

    catch(error) {
      // console.log(error);
      Shiny.setInputValue("b221-showError", "allFields", {priority: "event"});
    }

}

function saveNewCollection() {
  let state = $('#b221-slideInRight .collectionHeader')[0].id,
  hintId = $('#b221-slideInRight .removeslideinui')[0].id,
  starredHint = null,
  officialHint = [],
  childIds = [],
  discard_reasons = collectReasons();

    $("#hintContainer > div.hint-item").each((index, elem) => {
      if ($(elem).hasClass('starred')) {
        starredHint = elem.id.replace("hintId_","");
      }
      if ($(elem).hasClass('official')) {
        officialHint.push(elem.id.replace("hintId_",""));
      }
    childIds.push(parseInt(elem.id.replace("hintId_","")));
  });

  Shiny.setInputValue("b221-saveNewCollection", JSON.stringify({childIds, state, hintId, starredHint, officialHint, discard_reasons}),    {priority: "event"});
}


function discardExistingCollection() {
  var state = $('#b221-slideInRight .collectionHeader').length > 0 ? $('#b221-slideInRight .collectionHeader')[0].id : null;
  let reasons = collectReasons();
  console.log(reasons)
  console.log(state)
  if (state == null) {
    let id = $('#confirm-discard').attr('class').match(/(?<=leadsID_).*/gi)[0];
    del_or_dis('Discard','hint','Mark irrelevant');
    Shiny.setInputValue("b221-discardSingleCollection", JSON.stringify({ reasons, id: id }), {priority: "event"});
  } else {
    let del_dis = $('#confirm-discard > div > div.discard-select-fields').css('display') == 'none' ? 'Delete' : 'Discard';
    del_or_dis(del_dis);
    Shiny.setInputValue("b221-discardExistingCollection", JSON.stringify({ state: state, reasons: reasons, del_or_dis: del_dis}),             {priority: "event"});
    //del_or_dis == 'Delete' ? : saveNewCollection(discard_reasons = reasons);
  }

}

var collectReasons = function(){
  let select = $('#b221-discardSelect').val(),
      other = $('#b221-discardOther').val(),
      reasons = Object.assign({},
        select.length == 0 ? null : {select},
        other === '' ? null : {other}
      );
  reasons = jQuery.isEmptyObject(reasons) ? null : reasons;
  return reasons;
}

var del_or_dis = function(type = 'Discard', hintCollection = 'Hint', buttonName = 'Mark irrelevant') {
    if (type == 'Delete'){
      $('.confirm-discard-inner > p').text(`You are deleting this ${hintCollection}`)
      $('#b221-discardCollection i')[0].className = '';
      $('#b221-discardCollection i').addClass('fa fa-trash');
    } else {
      $('.confirm-discard-inner > p').text(`You are marking this ${hintCollection} as irrelevant`)
      $('#b221-discardCollection i')[0].className = '';
      $('#b221-discardCollection i').addClass('fa fa-times');
    }
    $('#b221-discardCollection').html(function(){ return $(this).html().replace(/Delete|Discard/gi, buttonName) })
}

var clear_discard = function(){
      $('#confirm-discard').removeClass('show');
      
      $('#confirm-discard').removeClass (function (index, className) {
                return (className.match (/leadsID_.*/gi) || []).join(' ');
              });
              $('#b221-discardSelect').selectize()[0].selectize.clear();
              $('#b221-discardOther').val('');
}