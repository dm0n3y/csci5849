rows = ["#row0", "#row1", "#row2", "#row3"];
buttons = [
	["#button7","#button8","#button9","#buttonDivide"],
	["#button4","#button5","#button6","#buttonMultiply"],
	["#button1","#button2","#button3","#buttonAdd"],
	["#button0","#buttonClear","#buttonEquals","#buttonSubtract"]
];
selectedRowIndex = -1;
selectedColIndex = -1;
numRows = rows.length;
numCols = buttons[0].length;
currentTimer = null;
loopingRows = false;

function deselectAll() {
	$("button").removeClass("cursor");
}

function selectRow(rowIndex) {
	deselectAll();
	$(`${rows[rowIndex]} button`).addClass("cursor");
	selectedRowIndex = rowIndex;
	selectedColIndex = -1;
}

function selectButton(rowIndex, colIndex) {
	deselectAll();
	$(buttons[rowIndex][colIndex]).addClass("cursor");
	selectedRowIndex = rowIndex;
	selectedColIndex = colIndex;
}

function selectNextRowItem() {
	if (selectedColIndex == numCols - 1) {
		selectRow(selectedRowIndex);
	} else {
		selectButton(selectedRowIndex, (selectedColIndex + 1) % numCols);
	}
}

function selectNextRow() {
	selectRow((selectedRowIndex + 1) % numRows);
}

function clickSelectedRowItem() {
	if (selectedColIndex < 0) {
		loopRows();
	} else {
		$(buttons[selectedRowIndex][selectedColIndex]).click();
	}
}

function clickSelectedRow() {
	loopRowItems();
}

function loopRows() {
	window.clearInterval(currentTimer);
	currentTimer = window.setInterval(selectNextRow, 400);
	loopingRows = true;
}

function loopRowItems() {
	window.clearInterval(currentTimer);
	currentTimer = window.setInterval(selectNextRowItem, 400);
	loopingRows = false;
}

// this function responds to user key presses
// you'll rewrite this to control your interface using some number of keys
$(document).keypress(function(event) {
	if (event.key == " " && loopingRows) {
		clickSelectedRow();
	} else if (event.key == " " && !loopingRows) {
		clickSelectedRowItem();
	}
})


/* calculator stuff below here */
// for operations, we'll save + - / *
firstValue = null;
operation = null;
addingNumber = false;

digits = "0123456789"
operators = "+-*/"

// handle calculator functions. all buttons with class calcButton will be handled here
$(".calcButton").click(function(event) {
	buttonLabel = $(this).text();
	
	// if it's a number, add it to our display
	if (digits.indexOf(buttonLabel) != -1) {
		// if we weren't just adding a number, clear our screen
		if (!addingNumber) {
			$("#number_input").val("")
		}
		$("#number_input").val($("#number_input").val() + buttonLabel);
		addingNumber = true;
	// if it's an operator, push the current value onto the stack
	} else if (operators.indexOf(buttonLabel) != -1) {
		// have we added a number? if so, check our stack
		if (addingNumber) {
			// is this the first number on the stack?
			// if so, save it
			if (firstValue == null) {
				firstValue = $("#number_input").val();
				addingNumber = false;
			// do we have a number on the stack already? if so, this is the same as equals
			} else if (firstValue != null) {
				secondValue = $("#number_input").val();
				evaluateExpression(firstValue,operation,secondValue)
				// in this case, keep the operation
				firstValue = $("#number_input").val();
				addingNumber = false;
			}
		}
		// either way, save this as the most recent operation
		operation = buttonLabel;
	} else if (buttonLabel == "C") {
		$("#number_input").val("");
		firstValue = null;
		operation = null;
		addingNumber = false;
	} else if (buttonLabel == "=") {
		if (firstValue != null && operation != null && addingNumber) {
			secondValue = $("#number_input").val();
			evaluateExpression(firstValue,operation,secondValue);
			// clear our state
			firstValue = null;
			operation = null;
			addingNumber = true
		}
	}
})

// do the math for our calculator
function evaluateExpression(first,op,second) {
	output = 0;
	if (op == "+") {
		output = parseInt(first) + parseInt(second);
	} else if (op == "-") {
		output = parseInt(first) - parseInt(second);
	} else if (op == "*") {
		output = parseInt(first) * parseInt(second);
	} else if (op == "/") {
		output = parseInt(first) / parseInt(second);
	}
	
	// now, handle it
	$("#number_input").val(output.toString());
	// deal with state elsewhere
}

$(document).ready(function() {
	loopingRows = true;
	loopRows();
});
