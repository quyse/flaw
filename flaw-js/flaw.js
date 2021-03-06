function h$flaw_js_init(callback) {
	// asyncronously pass control to callback, so document's body can be loaded
	setTimeout(callback, 0);
}

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

// get params map from URL
function h$flaw_js_get_url_params() {
	var pieces = window.location.search.substr(1).split('&');
	var keyValues = {};
	for(var i = 0; i < pieces.length; ++i) {
		var keyValue = pieces[i].split('=');
		keyValues[decodeURIComponent(keyValue[0])] = decodeURIComponent(keyValue[1]);
	}
	return keyValues;
}

function h$flaw_js_blob_to_array_buffer(blob, callback) {
	var reader = new FileReader();
	reader.addEventListener("loadend", function() {
		callback(reader.result);
	});
	reader.readAsArrayBuffer(blob);
}

function h$flaw_js_load_url(url, callback) {
	var xhr = new XMLHttpRequest();
	xhr.open('GET', url, true);
	xhr.responseType = 'arraybuffer';
	xhr.onload = function(e) {
		callback(this.status == 200 ? this.response : new ArrayBuffer(0));
	};
	xhr.send();
}

function h$flaw_js_object_url(mime, object) {
	return URL.createObjectURL(new Blob([object], {
		type: mime
	}));
}
