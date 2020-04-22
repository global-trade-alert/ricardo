// FUNCTIONS

// REACTIVATING LEADS ITEM
function checkLeads() {
  $('#osc-leadsTable').on('click', '.leads-item .right-col .evaluate',function (e) {
   // Check if sender is the <div> element e.g.
   console.log('checkLeads');
    var elementType = $(this)[0].id
    console.log(elementType);
    
    var elementID = $(this.parentNode.parentNode.parentNode)[0].id
    console.log(elementID);
    
    var changeEl = $(this.parentNode.parentNode.parentNode)
    // console.log(elementID);
    
    if(!$(e.target).is('#osc-leadsTable .no-touch')) {
      if (elementType == "discard" && ! $(this).hasClass('dismiss')) {
          $(changeEl).removeClass('reactivate');
          $(changeEl).addClass('dismiss');
          Shiny.setInputValue("osc-checkLeadsClick", [elementType, elementID], {priority: "event"});
      } else if (elementType == "relevant" && ! $(this).hasClass('reactivate')) {
          $(changeEl).removeClass('dismiss');
          collectData(`#${elementID}`);

      }
  }

  });
}

// REACTIVATING LEADS ITEM
function checkLeadsManual() {
  $('#osc-leadsTable').on('click', '.leads-item .right-col .evaluate',function (e) {
   // Check if sender is the <div> element e.g.
   console.log('checkLeadsManual');
    var elementType = $(this)[0].id
    
    var elementID = $(this.parentNode.parentNode.parentNode)[0].id
    
    var changeEl = $(this.parentNode.parentNode.parentNode)
    
    if(!$(e.target).is('#osc-leadsTable .no-touch')) {
      if (elementType == "discard" && ! $(this).hasClass('dismiss')) {
          $(changeEl).removeClass('reactivate');
          $(changeEl).addClass('dismiss');
          Shiny.setInputValue("osc-checkLeadsClick", [elementType, elementID], {priority: "event"});
      } else if (elementType == "relevant" && ! $(this).hasClass('reactivate')) {
          $(changeEl).removeClass('dismiss');
          console.log(`#${elementID}`);
          collectData(`#${elementID}`);
      } 
  }

  });
}

function basicUI() {

  // SHOW MORE BUTTON
  $('#osc-leadsTable').on('click', '.leads-item .show-more',function () {
    $(this.parentNode.parentNode.parentNode).toggleClass('showAll');
  });

  $('.backdrop-nav').on('click', function () {
    $('#osc-slideInRight').removeClass('open');
  });

  $('#osc-leadsTable').on('click', '.leads-item .collection-add',function () {
    var elementID = $(this.parentNode.parentNode.parentNode)[0].id
    if ($(this).hasClass('partOfCollection')){
      var collectionID = $(this)[0].id;
      var collection = true;
    } else if ($(this).hasClass('noPartOfCollection')) {
      var collectionID = false;
      var collection = false;
    } {
      
    }
    
    Shiny.setInputValue("osc-collectionAdd", [elementID, collectionID, collection], {priority: "event"});
    console.log("INPUT VALUE SET");
    // $('#osc-slideInRight').trigger('loadAllCollections');
    // console.log("COLLECTIONS ITEM:"+$('#osc-leadsTable #osc-loadCollections'));
    $('.backdrop-nav').toggleClass('open');
  });
  
  $('#osc-slideInRight').on('loadCollectionSlideIn',function () {
    var elementID = $(this)[0].children[0].id
    Shiny.setInputValue("osc-loadCollections", elementID, {priority: "event"});
    Shiny.setInputValue("osc-loadSingleHints", elementID, {priority: "event"});
    // console.log("COLLECTIONS ITEM:"+$('#osc-leadsTable #osc-loadCollections'));
    $('.backdrop-nav').addClass('open');
  });
  
  
  // SHOW COUNTRIES DROPDOWNS ON SELECT
  $('#osc-leadsTable').on('click','.top-row',function (event) {
    event.stopPropagation(); // prevent bubbling to .leads-item
  });
  
  $('#osc-leadsTable').on('click','.submission',function (event) {
    event.stopPropagation(); // prevent bubbling to .leads-item
  });

  $('#osc-leadsTable').on('change','input',function (event) {
    $(this.parentNode.parentNode.parentNode.parentNode.parentNode).addClass('show-submission');
  });
  
  $('#osc-leadsTable').on('change','.top-row select',function (event) {
    $(this.parentNode.parentNode.parentNode.parentNode.parentNode).addClass('show-submission');
  });
  
  $('#osc-leadsTable').on('change keyup paste','.comment input',function (event) {
    $(this.parentNode.parentNode.parentNode.parentNode).addClass('show-submission');
  });

  $('#osc-leadsTable').on('mouseenter', '.leads-item', function(event){
    console.log("MOUSENTER SUCCESS 3");
    // gather all select box classes => pass to object => pass to server
    let country_select = $('[id^=country]', this);
    let current_country = $(country_select).val();
    let elementID =  $(country_select).attr('id').replace('country_','');
  })
  
}

basicUI();

function removeHint() {
  $('#osc-slideInRight').on('click','.remove',function () {
    console.log($(this.parentNode));
    var removeElement = $(this.parentNode);
    removeElement.fadeOut(300, function(){
      $(this).remove();
    });
  });
}

// EVENT LISTENER FOR LEADS_ITEM ARRIVING AT TOP
function leadsDismiss() {
  console.log("FUNCTION LOADED");
    $('.leads-item').each(function () {
        var $this = $(this);
        var offset = $this.offset().top;
        var scrollTop = $(window).scrollTop();
        var elementID = $(this)[0].id
        var elementType = "relevant"
        // console.log('Scrolltop' + scrollTop);
        // console.log('offset' + offset);
        if (scrollTop > offset && !$(this).hasClass('checked')) {
          Shiny.setInputValue("osc-checkLeads", elementID, {priority: "event"});
          $(this).addClass('checked');
          // console.log("LEAD CHECKED");
          if (!$(this).hasClass('dismiss') ) {
            $(this).addClass('reactivate');
            Shiny.setInputValue("osc-checkLeadsClick", [elementType, elementID], {priority: "event"});
            // console.log("LEAD CHECKED");
          }
        }
    });
}

function callLeadsDismiss() {
  $(window).scroll(leadsDismiss);
}

// Submit data of one hint
function submitSingleHint() {
  console.log("submitSingleHint");
  $('#osc-leadsTable').on('click', '.leads-item [id^=submit]',function () {
    console.log(this)
     let id = $(this)[0].id.replace("submit_", "");
     collectData(`#leadsID_${id}`);
  });
}

//  Collect data for a general submit of hints
function collectData(type=''){
  console.log("Collector running");
  let selector = `${type}`;
  let output = [];
  try {
      $(selector).each(function () {
        console.log($(this));
        let clicked = 1 // add logics if hint is not selected at all
          let id = $(this)[0].id.replace("leadsID_", "");
          let country = $(`#country_${id}`).val().join(' ; ');
          let product = $(`#product_${id}`).val().join(' ; ');
          let intervention = $(`#intervention_${id}`).val().join(' ; ');
          let assessment = $(`#assessment_${id}`).val();
          let comment = $(`#comment_${id}`).val();
          let url = $(`#leadsID_${id} .background-url a`).attr('href');
          let official = $(`#official_${id}`).is(':checked') ? 1 : 0;
          output.push({id: id, clicked: clicked, country: country, product: product, intervention: intervention,
            assessment: assessment, official: official, comment: comment, url: url
          })
    });
    console.log(output)
    Shiny.setInputValue("osc-collectedData", JSON.stringify(output), {priority: "event"});
    $(`${type}`).addClass('reactivate');
    }
    catch(error) {
      console.log(error);
      Shiny.setInputValue("osc-showError", "allFields", {priority: "event"});
    }

}

function saveNewCollection() {
  var collectionID = $('#osc-slideInRight .newCollection')[0].id;
  var state = $('#osc-slideInRight .collectionHeader')[0].id;
  var childIds = [];
  
  $("#hintContainer > div").each((index, elem) => {
    childIds.push(parseInt(elem.id.replace("hintId_","")));
  });
  
  Shiny.setInputValue("osc-saveNewCollection", JSON.stringify({collectionID, childIds, state}), {priority: "event"});
}
