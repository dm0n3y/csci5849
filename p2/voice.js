let recognition = new webkitSpeechRecognition();
recognition.lang = 'en-US';
recognition.interimResults = false;
recognition.maxAlternatives = 1;

recognition.onnomatch = function() {
  console.log("I didn't recognize that command.");
}

recognition.onerror = function(event) {
  console.log(event.error);
}

recognition.onresult = function(event) {
  var command = event.results[0][0].transcript;
  console.log(command);
}

var interval;
var held = false;

$(document).on('keydown', function(e) {
  if (e.key == "Enter") {
    if (interval == null) {
      console.log('keydown');
      held = false;
      interval = setInterval(function() {
        if (!held) {
          handleEnterHeld(e.keyCode);
          held = true;
        }
      }, 100);
    }
  }
}).on('keyup', function(e) {
  if (e.key == "Enter") {
    console.log('keyup');
    clearInterval(interval); 
    interval = null;
    if (!held) {
      handleEnterTapped(e.keyCode);
    } else {
      handleEnterReleased(e.keyCode);
    }
  }
}).on('keypress', function(e) {
  if (e.key == " ") {
    handleSpacePressed();
  }
})

function handleEnterHeld(keyCode) {
  recognition.start();
}

function handleEnterReleased(keyCode) {
  recognition.stop();
}

function handleEnterTapped(keyCode) {
  console.log(`key tapped: ${keyCode}`);
}

function handleSpacePressed() {
  console.log("space");
}
