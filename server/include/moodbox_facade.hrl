-record(envelope, {header, body}).
-record(header, {auth_ticket, version_tag, version, language}).

-record(fault, {code, description, details}).

-record(server_info, {is_invitation_required, max_request_size}).

%% exceptions

-record(cannot_authorize_yourself_exception, {user_id}).

%% DB records
                                                                    
-record(user_account_data, {id, login, upper_login = "", password, creation_date, is_logon_allowed = true, motto = "", about_me = "", name = "", upper_name = "", country, city = "", email = "", upper_email = "", sex, birth_day, language, allow_news, allow_publishing, allow_show_friends = true, role}).
-record(user_account_sorted, {creation_date, id}).

-record(contact, {owner_id, items = [], channels = []}).
-record(contact_item, {contact_id, message, authorization_state, is_blocked = false}).

-record(art_message, {message_id, send_date, recepient_ids = [], delivery_state, author_id, is_public = false}).

-record(moodstrip, {id, author_id, author_login, send_date, title = "", upper_title = "", is_published = false, items = [], language, is_hidden = false, channel_id}).
-record(moodstrip_sorted, {creation_date, id}).
-record(moodstrip_sorted_ru, {creation_date, id}).

-record(published_message, {author_id, message_ids =[], message_image_ids =[]}).

-record(channel, {id, author_id, creation_date, title, upper_title, short_description, 
		  full_description, users = [], user_count = 0, order = 0, order_ru = 0, 
                  moderators = []}).
-record(channel_moderator, {moderator_id, channels = []}).
-record(contact_channel, {channel_id, is_blocked = false, is_deleted = false}).
-record(channel_message, {channel_id, messages = [], message_count}).
-record(channel_message_item, {message_id, author_id, author_login, send_date, obscene_count = []}).


-record(mailing_info, {user_id, last_message_id, unsubscribe_code}).

%% result records

-record(user_account, {id, login, password, creation_date, motto = "", about_me = "", name = "", country, city = "", email = "", sex, birth_day, language, allow_news, allow_publishing, allow_show_friends = true, userpic_url, role, moderate_contacts = []}).

-record(authorization, {state, message}).

-record(auth_ticket_result, {auth_ticket, user_id, lifetime, result_code, lock_time}).

-record(user_search_result, {page_number, has_more, items}).

-record(contact_info, {user_id, login, status, motto, name, birth_day,
                      authorization_state, message, is_blocked, type = friend}).
-record(artmessage_result, {message_id, type, author_id, send_date, data, result_code, metadata, url}).

-record(user_info, {user_id, login, name, creation_date, motto, about_me, country, city, sex, birth_day, userpic_url, status, show_tv}).

-record(user_picture_result, {result_code, user_id, last_change_date, picture_data}).

-record(contact_login, {contact_id, login}).

-record(published_images_result, {result_code, images}).
-record(published_image, {send_date, url, author_login}).

-record(moodstrip_item, {message_id, image_id, author_login, send_date}).

-record(moodstrip_item_result, {url, author_login, send_date}).

-record(moodstrip_result, {moodstrip_id, author_id, author_login, title, send_date, url, items, count}).

-record(moodstrip_search_result, {page_number, has_more, items}).

-record(publishing_moodstrip_result, {result_code, urls}).
-record(publishing_way, {code, url}).

-record(send_message_result, {result_code, message_id, send_date}).

-record(channel_result, {channel_id, author_id, author_login, creation_date, title,
			 short_description, full_description, user_count, logo_url, moderators}).
-record(channel_message_result, {result_code, message_id, author_id, author_login, send_date, data, metadata = []}).

-record(channel_message_url, {result_code, message_id, author_id, author_login, send_date, url}).

%% notification records

-record(notification_registration_result, {server, key}).

-record(notification_result, {packet_id, notifications}).

-record(notification, {user_id, event}).


%% result codes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% update/create account
-define(age_must_be_13_or_greater, age_must_be_13_or_greater).
-define(invalid_invite_code, invalid_invite_code).
-define(login_is_not_available, login_is_not_available).
-define(account_not_found, account_not_found).
-define(invalid_login, invalid_login).
-define(invalid_password, invalid_password).
-define(invalid_email, invalid_email).
-define(userpic_size_too_big, userpic_size_too_big).

%% authorization
-define(authorized_by_this_request, authorized_by_this_request).
-define(already_authorized, already_authorized).
-define(incorrect_db_data, incorrect_db_data).
-define(not_waiting_for_authorization, not_waiting_for_authorization).
-define(author_has_too_many_contacts, author_has_too_many_contacts).
-define(recipient_has_too_many_contacts, recipient_has_too_many_contacts).
-define(user_blocked_you, user_blocked_you).

%% contact
-define(contact_not_found, contact_not_found).
-define(hidden, hidden).
-define(empty, empty).
-define(closed_contact, closed_contact).

%% userpic
-define(not_modified, not_modified).
-define(not_found, not_found).

%% moodstrip
-define(moodstrip_not_found, moodstrip_not_found).
-define(moodstrip_is_published, moodstrip_is_published).
-define(moodstrip_is_empty, moodstrip_is_empty).
-define(is_saving, is_saving).
-define(saved, saved).

%% password reminder

%% unsubscribe
-define(unsubscribe_error, unsubscribe_error).

%% artmessage
-define(message_data_not_found, message_data_not_found).

%% notification
-define(invalid_key, invalid_key).
-define(create_packet, create_packet).
-define(retry_packet, retry_packet).
-define(session_is_not_exist, session_is_not_exist).
-define(notification_server_was_deleted, notification_server_was_deleted).

%% other codes, types, etc. %%%%%%%%%%%%%%%%%%%%%

%% types of message
-define(friends, friends).
-define(private, private).
-define(group, group).

%% authorization states
-define(authorized, authorized).
-define(waits_authorization_from_me, waits_authorization_from_me).
-define(not_authorized_me, not_authorized_me).

%date range
-define(today, today).
-define(week, week).
-define(month, month).
-define(all_time, all_time).

%sorted by
-define(most_recent, most_recent).

%user's status
-define(offline, offline).
-define(online, online).
-define(connecting, connecting). % for client code only

% notification events
-define(disconnect, disconnect).
-define(status_changed, status_changed).
-define(userpic_changed, userpic_changed).
-define(contact_changed, contact_changed).
-define(authorization_changed, authorization_changed).
-define(new_message, new_message).
-define(reload, reload).
-define(new_channel_message, new_channel_message).
-define(new_command, new_command).

% invitation
-define(limit_was_exceeded, limit_was_exceeded).

% newbie channel id
-define(NewbieChannelId, 1).
-define(NewbieChannelId_ru, 2).

% contact item's type
-define(friend, friend).
-define(channel, channel).

% channel user blocking
-define(forbidden, forbidden).

% user roles
-define(admin, admin).

% publishing ways
-define(simple_url, simple_url).
-define(embedded_flash, embedded_flash).
-define(lj, lj).
-define(lj_cut, lj_cut).
