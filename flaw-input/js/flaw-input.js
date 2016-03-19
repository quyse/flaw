// register input events
function h$flaw_input_register_events(canvas
	, keydownCallback
	, keyupCallback
	, keypressCallback
	, mousedownCallback
	, mouseupCallback
	, rawMouseMoveCallback
	, cursorMoveCallback
	) {
	document.addEventListener('keydown', function(e) {
		keydownCallback(e.which);
	}, false);
	document.addEventListener('keyup', function(e) {
		keyupCallback(e.which);
	}, false);
	document.addEventListener('keypress', function(e) {
		keypressCallback(e.which);
	}, false);
	canvas.addEventListener('mousedown', function(e) {
		mousedownCallback(e.button);
	}, false);
	canvas.addEventListener('mouseup', function(e) {
		mouseupCallback(e.button);
	}, false);

	var lastCursorX = undefined, lastCursorY = undefined;
	canvas.addEventListener('mousemove', function(e) {
		var movementX = e.movementX || 0;
		var movementY = e.movementY || 0;
		var cursorX = e.clientX;
		var cursorY = e.clientY;
		if(movementX != 0 || movementY != 0) {
			rawMouseMoveCallback(movementX, movementY, 0);
		}
		if(cursorX != lastCursorX || cursorY != lastCursorY) {
			cursorMoveCallback(cursorX, cursorY);
			lastCursorX = cursorX;
			lastCursorY = cursorY;
		}
	}, false);

	canvas.addEventListener('wheel', function(e) {
		var movementZ = e.wheelDelta || ((e.deltaY || 0) * 40);
		if(movementZ != 0) {
			rawMouseMoveCallback(0, 0, movementZ);
		}
	}, false);
}
