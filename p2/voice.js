const A = require('arcsecond');

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
  do_command(command);
  /*
  console.log(command);
  switch (command) {
    case "case":
    case "construct case":
      let construct_case = new Event("construct_case");
      document.dispatchEvent(construct_case);
      break;
    case "lambda":
    case "construct lambda":
    case "function":
    case "construct function":
      let construct_lam = new Event("construct_lam");
      document.dispatchEvent(construct_lam);
      break;
    case "let":
    case "construct let":
      let construct_let = new Event("construct_let");
      document.dispatchEvent(construct_let);
      break;
    case "wild":
    case "construct wild":
    case "underscore":
    case "construct underscore":
      let construct_wild = new Event("construct_wild");
      document.dispatchEvent(construct_wild);
      break;
    case "colon":
    case "construct colon":
      document.dispatchEvent(construct_asc);
      break;
    default:
      break;
  }
  */
}

let space = A.many1(A.str(" "));

let variable = //A.pipeParsers([
  A.sequenceOf([
    A.str("variable"),
    space,
    A.many(A.anythingExcept(A.whitespace))
  ]);
  /*
  A.mapTo(([variable, space, name]) => {
    [new CustomEvent("construct_var", { name })]
  })
  */
//  A.mapTo(strings => strings.join(" "))
//]);

let parser = A.choice([
  variable
]);

/*
let let_complex = A.pipeParsers([
  A.sequenceOf([
    let_simple,
    space,
    variable,
    space,
    equal,
    space,
    expression
  ]),
  mapTo(([lett, space, equal, expression]))
]);
*/

/*
function do_command(command) {
  let events = A.parse(parser).value;
  console.log(events);
}
*/

let backspace = new Event("backspace");
let del = new Event("delete");
let construct_case = new Event("construct_case");
let construct_lam = new Event("construct_lam");
let construct_let = new Event("construct_let");
let construct_asc = new Event("construct_asc");

let num_words = {
  "one": 1,
  "two": 2,
  "three": 3,
  "four": 4,
  "five": 5,
  "six": 6,
  "seven": 7,
  "eight": 8,
  "nine": 9,
  "ten": 10
};

function do_command(command) {
  let tokens = command.toLowerCase().split(" ");
  console.log(tokens);
  switch (tokens.length) {
    case 0:
      break;
    case 1:
      switch (tokens[0]) {
        case "backspace":
          document.dispatchEvent(backspace);
          break;
        case "delete":
          document.dispatchEvent(del);
          break;
        case "case":
          document.dispatchEvent(construct_case);
          break;
        case "function":
        case "lambda":
          document.dispatchEvent(construct_lam);
          break;
        case "define":
          document.dispatchEvent(construct_let);
          break;
        case "ascription":
          document.dispatchEvent(construct_asc);
          break;
        case "empty":
          document.dispatchEvent(new Event("construct_nil"));
          break;
        case "plus":
          document.dispatchEvent(new Event("construct_plus"));
          break;
        case "multiply":
          document.dispatchEvent(new Event("construct_times"));
          break;
        case "space":
          document.dispatchEvent(new Event("construct_space"));
          break;
        case "head":
          document.dispatchEvent(new Event("construct_cons"));
          break;
        case "line":
          document.dispatchEvent(new Event("construct_line"));
          break;
        case "parentheses":
          document.dispatchEvent(new Event("construct_paren"));
          break;
        default:
          break;
      }
      break;
    case 2:
      switch (tokens[0]) {
        case "variable":
          let construct_var = new CustomEvent("construct_var", { detail: tokens[1] });
          document.dispatchEvent(construct_var);
          break;
        case "number":
          let digit = num_words[tokens[1]];
          console.log("digit");
          console.log(digit);
          var detail;
          if (digit) {
            detail = digit;
          } else {
            detail = tokens[1];
          }
          let construct_num = new CustomEvent("construct_num", { detail: detail })
          document.dispatchEvent(construct_num);
          break;
        default:
          break;
      }
    default:
      break;
  }
}

var interval;
var held = false;

$(document).on('keydown', function(e) {
  if (e.key == " ") {
    if (interval == null) {
      held = false;
      interval = setInterval(function() {
        if (!held) {
          startRecording();
          held = true;
        }
      }, 100);
    }
  }
}).on('keyup', function(e) {
  if (e.key == " ") {
    clearInterval(interval); 
    interval = null;
    if (held) {
      stopRecording();
    }
  }
});

function startRecording() {
  recognition.start();
}

function stopRecording() {
  recognition.stop();
}
