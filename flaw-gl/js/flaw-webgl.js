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

// set number of draw buffers in framebuffer
function h$flaw_webgl_glDrawBuffers_n(n) {
	var buffers = [];
	for(var i = 0; i < n; ++i)
		buffers.push(h$flaw_wegbl_context.COLOR_ATTACHMENT + i);
	h$flaw_webgl_context.drawBuffers(buffers);
}

var h$requestAnimationFrame
	= window.requestAnimationFrame
	|| window.webkitRequestAnimationFrame
	|| window.mozRequestAnimationFrame
	|| window.msRequestAnimationFrame
	;
