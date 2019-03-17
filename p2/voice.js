let recognition = new webkitSpeechRecognition();
recognition.lang = 'en-US';
recognition.interimResults = false;
recognition.maxAlternatives = 1;

/*
document.body.onclick = function() {
  recognition.start();
  console.log("Ready to receive voice commands.")
}
*/

recognition.onspeechend = function() {
  recognition.stop();
}

recognition.onnomatch = function() {
  console.log("I didn't recognize that command.");
}

recognition.onerror = function(event) {
  console.log(event.error);
}

recognition.onresult = function(event) {
  var command = event.results[0][0].transcript;
  console.log(command);
  recognition.stop();
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
          handleKeyHeld(e.keyCode);
          held = true;
        }
      }, 1000);
    }
  }
}).on('keyup', function(e) {
  if (e.key == "Enter") {
    console.log('keyup');
    clearInterval(interval); 
    interval = null;
    if (!held) {
      handleKeyTapped(e.keyCode);
    } else {
      handleKeyReleased(e.keyCode);
    }
  }
});

function handleKeyHeld(keyCode) {
  console.log(`key held: ${keyCode}`);
}

function handleKeyReleased(keyCode) {
  console.log(`key released: ${keyCode}`);
}

function handleKeyTapped(keyCode) {
  console.log(`key tapped: ${keyCode}`);
}
