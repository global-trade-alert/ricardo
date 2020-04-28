// FUNCTIONS




// REACTIVATING LEADS ITEM
function checkLeads() {
  $('#b221-leadsTable').on('click', '.leads-item .right-col .evaluate',function (e) {
   // Check if sender is the <div> element e.g.
   console.log('checkLeads');
    var elementType = $(this)[0].id
    
    var elementID = $(this.parentNode.parentNode.parentNode)[0].id
    
    var changeEl = $(this.parentNode.parentNode.parentNode)
    
    if(!$(e.target).is('#b221-leadsTable .no-touch')) {
      if (elementType == "discard" && ! $(this).hasClass('dismiss')) {
          $(changeEl).removeClass('reactivate');
          $(changeEl).addClass('dismiss');
          // Shiny.setInputValue("b221-checkLeadsClick", [elementType, elementID], {priority: "event"});
          collectData(`#${elementID}`,'dismiss');
      } else if (elementType == "relevant" && ! $(this).hasClass('reactivate')) {
          $(changeEl).removeClass('dismiss');
          console.log(`#${elementID}`);
          collectData(`#${elementID}`,'reactivate');
      } 
  }

  });
}

// REACTIVATING LEADS ITEM
function checkLeadsManual() {
  $('#b221-leadsTable').on('click', '.leads-item .right-col .evaluate',function (e) {
   // Check if sender is the <div> element e.g.
   console.log('checkLeadsManual');
    var elementType = $(this)[0].id
    
    var elementID = $(this.parentNode.parentNode.parentNode)[0].id
    
    var changeEl = $(this.parentNode.parentNode.parentNode)
    
    if(!$(e.target).is('#b221-leadsTable .no-touch')) {
      if (elementType == "discard" && ! $(this).hasClass('dismiss')) {
          $(changeEl).removeClass('reactivate');
          $(changeEl).addClass('dismiss');
          // Shiny.setInputValue("b221-checkLeadsClick", [elementType, elementID], {priority: "event"});
          collectData(`#${elementID}`,'dismiss');
      } else if (elementType == "relevant" && ! $(this).hasClass('reactivate')) {
          $(changeEl).removeClass('dismiss');
          console.log(`#${elementID}`);
          collectData(`#${elementID}`,'reactivate');
      } 
  }

  });
}

function hintsBasicUI() {
  
  console.log("HINTS BASIC UI BOUND");

  // SHOW MORE BUTTON
  $('#b221-leadsTable').on('click', '.leads-item .middle-row',function () {
    $(this.parentNode.parentNode.parentNode).toggleClass('showAll');
  });

  $('#b221-leadsTable').on('click', '.leads-item .collection-add',function () {
    console.log("1 pressing collection-add button");
    var elementID = $(this.parentNode.parentNode.parentNode)[0].id
    if ($(this).hasClass('partOfCollection')){
      var collectionID = $(this)[0].id;
      var collection = "TRUE";
    } else if ($(this).hasClass('noPartOfCollection')) {
      var collectionID = "FALSE";
      var collection = "FALSE";
    }
    console.log([elementID, collectionID, collection]);
    
    Shiny.setInputValue("b221-collectionAdd", [elementID, collectionID, collection], {priority: "event"});
    Shiny.setInputValue("b221-loadCollections", elementID.replace("leadsID_",""), {priority: "event"});
    Shiny.setInputValue("b221-loadSingleHints", elementID, {priority: "event"});

  });
  
  
  $('#b221-slideInRight').on('loadCollectionSlideIn',function () {
    var elementID = $(this)[0].children[0].id
    console.log("LOAD COLLECTIONS SLIDE IN")
    console.log(elementID);
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
    console.log("MOUSENTER SUCCESS 3");
    // gather all select box classes => pass to object => pass to server
    let country_select = $('[id^=country]', this);
    let current_country = $(country_select).val();
    let elementID =  $(country_select).attr('id').replace('country_','');
  })
  
}

function slideInBasicUI() {
  
  $('.backdrop-nav').on('click', function () {
    $('#b221-slideInRight').removeClass('open');
  });
  
}


function removeHint() {
  $('#b221-slideInRight').on('click','.remove',function () {
    console.log($(this.parentNode));
    var removeElement = $(this.parentNode);
    removeElement.fadeOut(300, function(){
      $(this).remove();
    });
  });
}

// EVENT LISTENER FOR LEADS_ITEM ARRIVING AT TOP
function leadsDismiss() {
  console.log("LEADS DISMISSED FUNCTION LOADED");
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
  console.log('LEADS DISMISSED CALLED');
  $(window).scroll(leadsDismiss);
}

// Submit data of one hint
function submitSingleHint() {
  console.log("submitSingleHint");
  $('#b221-leadsTable').on('click', '.leads-item [id^=submit]',function () {
     console.log(this)
     let state = $(this.parentNode.parentNode.parentNode).hasClass('dismiss') ? "dismiss" : "reactivate";
     console.log(state);
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
  console.log(state);
  console.log(state=="reactivate");
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
          output.push({id: id, clicked: clicked, country: country, product: product, intervention: intervention,
            assessment: assessment, official: official, comment: comment, url: url
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
      console.log(error);
      Shiny.setInputValue("b221-showError", "allFields", {priority: "event"});  
    }

}

function saveNewCollection() {
  var state = $('#b221-slideInRight .collectionHeader')[0].id;
  var hintId = $('#b221-slideInRight .removeslideinui')[0].id;
  console.log("COLLETION HINT ID: "+hintId);
  
  var childIds = [];
  
  $("#hintContainer > div").each((index, elem) => {
    childIds.push(parseInt(elem.id.replace("hintId_","")));
  });
  
  Shiny.setInputValue("b221-saveNewCollection", JSON.stringify({childIds, state, hintId}), {priority: "event"});
}
