// current WebGL context
var h$flaw_webgl_context = null;

// get WebGL context from canvas
function h$flaw_webgl_get_canvas_context(canvas, needDepth) {
	var settings =
		{ alpha: false
		, depth: needDepth
		, stencil: false
		, antialias: true
		};
	return canvas.getContext('webgl', settings)
		|| canvas.getContext('experimental-webgl', settings);
}

var h$requestAnimationFrame
	= window.requestAnimationFrame
	|| window.webkitRequestAnimationFrame
	|| window.mozRequestAnimationFrame
	|| window.msRequestAnimationFrame
	;
