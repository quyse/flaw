// init canvas
function h$flaw_window_init_canvas() {
	var canvas = document.createElement('canvas');
	canvas.style.position = 'absolute';
	canvas.style.left = 0;
	canvas.style.top = 0;
	canvas.width = window.innerWidth;
	canvas.height = window.innerHeight;
	document.body.appendChild(canvas);
	document.body.style.overflow = 'hidden';
	window.addEventListener('resize', function() {
		canvas.width = window.innerWidth;
		canvas.height = window.innerHeight;
	}, false);
	return canvas;
}
