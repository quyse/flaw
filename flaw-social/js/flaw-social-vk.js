function h$flaw_social_vk_init_iframe(callback) {
	VK.init(function() {
		var pieces = window.location.search.substr(1).split('&');
		var keyValues = {};
		for(var i = 0; i < pieces.length; ++i) {
			var keyValue = pieces[i].split('=');
			keyValues[decodeURIComponent(keyValue[0])] = decodeURIComponent(keyValue[1]);
		}
		h$flaw_social_vk_viewer_id = keyValues.viewer_id;
		h$flaw_social_vk_auth_key = keyValues.auth_key;

		callback(true);
	}, function() {
		callback(false);
	}, '5.52');
}

var h$flaw_social_vk_viewer_id = '';
var h$flaw_social_vk_auth_key = '';