%% list of tables
-define(NOTIFICATION_SERVER_TABLE_LIST, [notification_server, session, notification_error]).
-define(SERVER_TABLE_LIST, [user_account_data, user_account_sorted, art_message, published_message, delivery, contact,
    		    logon_info, user_password_reset_request, sequence, moodstrip, moodstrip_sorted, 
		    moodstrip_sorted_ru, invitation, channel, channel_message, channel_moderator]).

%% common records
-record(notification_server, {pid, ip, modified_date}).
-record(session, {user_id, key, server_ip, server_pid, status}).
-record(notification_error, {user_id, server_pid, creation_date}).

%% for facade
-record(delivery, {recipient_id, count = {0, 0}, message_ids = [], last_got_author_id, commands = []}).
-record(queue_item, {message_id, type, author_id, send_date}).
-record(command_package, {package_id, items = []}).

%% command types
-record(delete_message_command, {contact_id, message_id}).

%% for invitation
-record(invitation_prefix, {prefix, count}).
-record(invitation, {email, code = "", prefix, created_date, used_date, state}).

%% get contact's logins for web-server
-record(contact_logins_result, {result_code, users, channels}).

%% result codes

-define(invalid_notification_key, invalid_notification_key).
-define(authorization_request_declined, authorization_request_declined).
-define(notify, notify).
