function h$flaw_social_fakebook_init() {
	var keyValues = h$flaw_js_get_url_params();
	h$flaw_social_fakebook_user_id = keyValues.fakebook_user_id || '';
}

var h$flaw_social_fakebook_user_id = '';
