let contacts = [{
  name: "Ben Shapiro",
  phone: "3034928570",
  email: "ben.shapiro@colorado.edu",
  sprite: "pikachu"
},{
  name: "Shaun Kane",
  phone: "3037357209",
  email: "shaun.kane@colorado.edu",
  sprite: "squirtle"
}];

$(document).ready(function() {
  makeActive($(".tab:contains(Dialer)"));
  
  for (let contact of contacts) {
    addContactElt(contact);
  }

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
    return false;
  });

  $("#add-contact-clear").click(function(e) {
    clearForm();
    return false;
  });
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
  let $table = $("#content-contact-list table");
  $("#content-contact-list table tbody")
    .append(`<tr>
      <td><img src="img/${contact.sprite}.png" alt="${contact.sprite}"></td>
      <td>${contact.name}</td>
      <td>${contact.phone}</td>
      <td>${contact.email}</td>
    </tr>`);
}

function clearForm() {
  $("#add-contact").find("#add-contact-name").val("");
  $("#add-contact").find("#add-contact-phone").val("");
  $("#add-contact").find("#add-contact-email").val("");
}
