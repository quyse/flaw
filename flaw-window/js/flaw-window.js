// init canvas
function h$flaw_window_init_canvas() {

	// create canvas
	var canvas = document.createElement('canvas');

	// set styles to be full-window
	canvas.style.position = 'absolute';
	canvas.style.left = 0;
	canvas.style.top = 0;
	canvas.width = window.innerWidth;
	canvas.height = window.innerHeight;

	// add canvas to page
	document.body.appendChild(canvas);
	document.body.style.overflow = 'hidden';

	// ensure canvas is always full-window
	window.addEventListener('resize', function() {
		canvas.width = window.innerWidth;
		canvas.height = window.innerHeight;
	}, false);

	// state of mouse lock
	var mouseLock = false;

	// method to set mouse lock
	canvas.flaw_window_set_mouse_lock = function(newMouseLock) {
		if(mouseLock == newMouseLock) return;
		mouseLock = newMouseLock;
		if(mouseLock) setPointerLock();
		else resetPointerLock();
	}

	function setPointerLock() {
		canvas.requestPointerLock();
	}
	function resetPointerLock() {
		document.exitPointerLock();
	}

	// check pointer lock on mousedown
	canvas.addEventListener('mousedown', function(e) {
		// only trying to set pointer lock, not reset, and only if needed
		if(mouseLock && h$flaw_window_pointerLockElement !== canvas) {
			setPointerLock();
		}
	}, false);

	// fixup canvas methods
	h$flaw_js_vendor_fixup_method(canvas, 'requestPointerLock');

	return canvas;
}

// fixup document methods
h$flaw_js_vendor_fixup_method(document, 'exitPointerLock');

// currently pointer-locked element
var h$flaw_window_pointerLockElement = null;
h$flaw_js_vendor_fixup_addEventListener(document, 'pointerlockchange', function() {
	h$flaw_window_pointerLockElement = h$flaw_js_vendor_fixup_read(document, 'pointerLockElement');
}, false);
