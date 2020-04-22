$(document).ready(function() {

// END OF INIT

// MENU
$('.menu-toggle').on('click', function () {
  if ($('main-menu-wrap.open')[0]) {
  $('.main-menu-wrap').removeClass('open');
  $('.backdrop-nav').removeClass('open');
  $('.header').removeClass('open');
} else {
  $('.main-menu-wrap').addClass('open');
  $('.backdrop-nav').addClass('open');
  $('.header').addClass('open');
}
});

$('.nav-items').on('click', function () {
  $('.main-menu-wrap').removeClass('open');
  $('.backdrop-nav').removeClass('open');
  $('.header').removeClass('open');
});

$('.active-menu-item').on('click', function () {
  if ($('main-menu-wrap.open')[0]) {
  $('.main-menu-wrap').removeClass('open');
  $('.backdrop-nav').removeClass('open');
  $('.header').removeClass('open');
} else {
  $('.main-menu-wrap').addClass('open');
  $('.backdrop-nav').addClass('open');
  $('.header').addClass('open');
}
});

$('.backdrop-nav').on('click', function () {
  $('.main-menu-wrap').removeClass('open');
  $('.backdrop-nav').removeClass('open');
  $('.header').removeClass('open');
});


});

// MAKE ENTER WORK FOR FORM ENTRIES
$(document).keyup(function(event) {
  if ($("#userInp").is(":focus") || $("#pwInp").is(":focus")) {
  if (event.key == "Enter") {
    $("#submitLogin").click();
  }}
});


// FUNCTIONS

// TOGGLE ACTIVE CLASSES FOR POP-UPS
// function toggleActive(label) {
//   Shiny.unbindAll();
//   $('#'+label).on('click', function () {
//     // var x = document.getElementsByClassName('.actionbutton-pop-up');
//     $('.actionbutton-pop-up').toggleClass('active');
//     console.log('POP_UP_22222');
//     // console.log(x);
//   });
//   Shiny.bindAll();
// }

function fixHeader() {
      $('.header').each(function() {
        // var offset = $('.header').offset().top;
        var offset = 10;
        var $this = $(this);
        var wh = window.innerHeight * 0.5;
        var scrollTop = $(window).scrollTop();
        // console.log("SCROLLTOP:"+scrollTop);
        // console.log("offset:"+offset);

        if (scrollTop > offset) {
          $this.addClass('sticky');
        } else if (scrollTop < offset) {
          $this.removeClass('sticky');
        }
      });
    }
    $(window).scroll(fixHeader);


function unbindAll() {
  Shiny.unbindAll();
}

function bindAll() {
  Shiny.bindAll();
  console.log("BOUND ALL");
}

function editDeleteApps() {
  $('#appList').on('click', '.editApp',function () {
    var elementID = $(this.parentNode)[0].id
    Shiny.setInputValue("settings-editApp", elementID, {priority: "event"});
  });
  
  $('#appList').on('click', '.deleteApp',function () {
    var elementID = $(this.parentNode)[0].id
    Shiny.setInputValue("settings-deleteApp", elementID, {priority: "event"});
  });
}

// CLOSE POP UP WHEN PRESSING background
function removePopUp() {
$('.pop-up-bg.active').on('click', function () {
    $('.pop-up-bg').removeClass('active');
});
}
