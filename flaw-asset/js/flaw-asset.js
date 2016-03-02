function h$flaw_asset_get_asset(url, callback) {
	var xhr = new XMLHttpRequest();
	xhr.open('GET', url, true);
	xhr.responseType = 'arraybuffer';
	xhr.onload = function(e) {
		callback(this.status == 200 ? this.response : new ArrayBuffer(0));
	};
	xhr.send();
}
