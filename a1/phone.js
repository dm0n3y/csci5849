$(document).ready(function() {
  makeActive($(".tab:contains(Dialer)"));

  $(".tab").click(function() {
    makeActive($(this));
  });

  $("#num-blobs .blob").click(function() {
    let $display = $("#num-display");
    let val = $display.val();
    let key = $(this).attr("data-key");
    $display.val(`${val}${key}`);
  });

  $("#dial-blobs .blob").click(function() {
    $("#num-display").val("");
  });

  $("#add-contact").submit(function(e) {
    e.preventDefault();
    var newContact = {};
    $(this).serializeArray().forEach((item) => {
      newContact[item.name] = item.value;
    });
    addContactElt(newContact);
    clearForm();
  });

  $("#add-contact-clear").click(clearForm);
});

function makeActive(tabElt) {
  // tab appearance
  $(".tab").removeClass("active");
  tabElt.addClass("active");
  // content appearance
  $("#content > div").hide();
  $(`#${tabElt.attr('content-id')}`).show();
}

function addContactElt(contact) {
  $("#content-contact-list").append(`<div class="blob">${contact.name}</div>`);
}

function clearForm() {
  $("#add-contact").find("input").val("");
}
