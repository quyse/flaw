// read field using vendor prefix if needed
function h$flaw_js_vendor_fixup_read(object, field) {
	var r = object[field];
	if(r === undefined) {
		var Field = field[0].toUpperCase() + field.substr(1);
		r = object["webkit" + Field] || object["moz" + Field] || object["o" + Field] || object["ms" + Field];
	}
	return r;
}

// fixup object's method using vendor prefix if needed
function h$flaw_js_vendor_fixup_method(object, method) {
	if(!object[method]) {
		object[method] = h$flaw_js_vendor_fixup_read(object, method);
	}
}

// fixup object's event using vendor prefix if needed
function h$flaw_js_vendor_fixup_addEventListener(object, event, listener, options) {
	object.addEventListener((("on" + event) in object) ? event
		: (("onwebkit" + event) in object) ? ("webkit" + event)
		: (("onmoz" + event) in object) ? ("moz" + event)
		: (("ono" + event) in object) ? ("o" + event)
		: (("ms" + event) in object) ? ("ms" + event)
		: event, listener, options);
}
