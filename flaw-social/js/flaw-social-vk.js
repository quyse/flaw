function h$flaw_social_vk_init_iframe(callback) {
	VK.init(function() {
		var keyValues = h$flaw_js_get_url_params();
		h$flaw_social_vk_viewer_id = keyValues.viewer_id || '';
		h$flaw_social_vk_auth_key = keyValues.auth_key || '';

		callback(true);
	}, function() {
		callback(false);
	}, '5.52');
}

var h$flaw_social_vk_viewer_id = '';
var h$flaw_social_vk_auth_key = '';
