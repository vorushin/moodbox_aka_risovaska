-module(moodbox_model_definition).

-export([get_definition/0]).

-include("moodbox_model.hrl").
-include("moodbox_facade.hrl").

get_definition() ->
    #model{
		name = 'MoodBox',

		auth_free_functions = [get_server_info, get_auth_ticket, create_account, get_user_info, reset_password, simple_search_contacts, advanced_search_contacts, get_sorted_users, get_contact_logins, get_moodstrip, search_moodstrip, get_last_moodstrips, get_sorted_moodstrips, get_contact_moodstrips, get_published_images, get_recovered_password, unsubscribe, get_srv_address, search_channel, get_channel_info, get_contact_moodstrips_4rss],

		types_info = [
			      ?Struct(1, envelope, [?FieldOptional(1, header, header), 
						    ?Field(2, body, #union{types = [
										    fault,
										    get_server_info, get_server_info_result,
										    get_auth_ticket, get_auth_ticket_result,
										    create_account, create_account_result,
										    update_account, update_account_result,
										    get_user_picture, get_user_picture_result,
										    get_contacts, get_contacts_result,
										    process_authorization_request, process_authorization_request_result,
										    process_authorization_request_by_login, 
										    process_authorization_request_by_login_result,
										    process_authorization_response, process_authorization_response_result,
										    remove_from_contacts, remove_from_contacts_result,
										    send_friend_message, send_friend_message_result,
										    send_private_message, send_private_message_result,
										    get_next_artmessage_and_report, get_next_artmessage_and_report_result,
										    block_contact, block_contact_result,
										    unblock_contact, unblock_contact_result,
										    simple_search_contacts, simple_search_contacts_result,
										    advanced_search_contacts, advanced_search_contacts_result,
										    update_password, update_password_result,
										    get_user_info, get_user_info_result,
										    get_user_info_by_login, get_user_info_by_login_result,
										    get_published_images, get_published_images_result,
										    get_contact_logins, get_contact_logins_result,
										    create_moodstrip, create_moodstrip_result,
										    add_picture_to_moodstrip, add_picture_to_moodstrip_result,
										    delete_moodstrip, delete_moodstrip_result,
										    get_moodstrip, get_moodstrip_result,
										    search_moodstrip, search_moodstrip_result,
										    get_last_moodstrips, get_last_moodstrips_result,
										    get_sorted_moodstrips, get_sorted_moodstrips_result,
										    get_sorted_users, get_sorted_users_result,
										    reset_password, reset_password_result,
										    get_recovered_password, get_recovered_password_result,
										    unsubscribe, unsubscribe_result,
										    get_contact_moodstrips, get_contact_moodstrips_result,
										    get_my_account, get_my_account_result,
										    notification_register, notification_register_result,
										    notification_unregister, notification_unregister_result,
										    get_notifications, get_notifications_result,
										    get_contact, get_contact_result,
										    get_status, get_status_result,
										    get_authorization, get_authorization_result,
										    get_srv_address, get_srv_address_result,
										    get_notification_timeout, get_notification_timeout_result,
										    check_invitation, check_invitation_result,
										    search_channel, search_channel_result,
										    get_channel_info, get_channel_info_result,
										    send_channel_message, send_channel_message_result,
										    get_next_channel_message, get_next_channel_message_result,
										    get_next_channel_message_url, get_next_channel_message_url_result,
										    add_user_to_channel, add_user_to_channel_result,
										    delete_user_from_channel, delete_user_from_channel_result,
										    update_channel, update_channel_result,
										    obscene_channel_message, obscene_channel_message_result,
										    delete_message, delete_message_result,
										    get_commands, get_commands_result,
										    get_contact_moodstrips_4rss, get_contact_moodstrips_4rss_result
										   ]})]),
			      
			      ?Struct(2, header, [?FieldOptional(1, auth_ticket, binary), ?FieldOptional(2, version_tag, version_tag), 
						  ?FieldOptional(3, version, string), ?FieldOptional(4, language, language)]),
			      ?Fault(3, fault, [?Field(1, code, string), ?Field(2, description, string), ?FieldOptional(3, details, string)]),

			      ?Struct(22, server_info, [?Field(1, is_invitation_required, bool), ?Field(2, max_request_size, int)]),
			      
			      ?Struct(4, user_account, [?Field(1, id, int), ?Field(2, login, string), ?Field(3, password, string), 
							?Field(4, creation_date, datetime), ?Field(5, motto, string), ?Field(6, about_me, string), 
							?Field(7, name, string), ?Field(8, country, country), ?Field(9, city, string), 
							?Field(10, email, string), ?Field(11, sex, sex), ?Field(12, birth_day, datetime, ?CppType(date)), 
							?FieldOptional(13, language, language), ?Field(14, allow_news, bool), 
							?Field(15, allow_publishing, bool), ?Field(16, allow_show_friends, bool),
							?FieldOptional(17, userpic_url, string),
							?FieldOptional(18, role, role),
							?FieldOptional(19, moderate_contacts, #list{type = int})],
						        #cpp_class_options{make_intermediate = true}),
			      ?Struct(6, contact_info, [?Field(1, user_id, int), ?Field(2, login, string), ?Field(3, status, user_status),
							?Field(4, motto, string), ?Field(5, name, string), ?Field(6, birth_day, datetime, ?CppType(date)), 
							?Field(7, authorization_state, authorization_state), ?Field(8, message, string), 
							?Field(9, is_blocked, bool), ?Field(10, type, contact_type)], 
				      #cpp_class_options{make_intermediate = true}),
			      ?Struct(7, art_message,  [?Field(1, message_id, int), ?Field(2, type, message_type), 
							?Field(3, author_id, int), ?Field(4, send_date, datetime), 
							?Field(5, data, binary), ?Field(6, result_code, artmessage_result_code),
							?Field(7, metadata, string), ?FieldOptional(8, url, string)
						       ]),
			      ?Struct(8, user_info, [?Field(1, user_id, int), ?Field(2, login, string), ?Field(3, name, string),
						     ?Field(4, creation_date, datetime), ?Field(5, motto, string), 
						     ?Field(6, about_me, string), ?Field(7, country, country), 
						     ?Field(8, city, string), ?Field(9, sex, sex), ?Field(10, birth_day, datetime, ?CppType(date)),
						     ?Field(11, userpic_url, string), ?Field(12, status, user_status),
						     ?Field(13, show_tv, bool)
						    ], 
				      #cpp_class_options{make_intermediate = true}),

			      % result structs
			      ?Struct(5, user_picture_result, [?Field(1, result_code, user_picture_result_code), ?Field(2, user_id, int), 
							       ?Field(3, last_change_date, datetime), ?Field(4, picture_data, binary)]),
			      ?Struct(9, contact_login, [?Field(1, contact_id, int), ?Field(2, login, string)]),
			      ?Struct(10, published_image, [?Field(1, send_date, datetime), ?Field(2, url, string), ?Field(3, author_login, string)]),
			      ?Struct(11, publishing_moodstrip_result, [?Field(1, result_code, moodstrip_result_code),
									?Field(2, urls, #list{type = publishing_way})]),
			      ?Struct(32, publishing_way, [?Field(1, code, url_code), ?Field(2, url, string)]),
			      ?Struct(12, moodstrip_result, [?Field(1, moodstrip_id, int),
							     ?Field(2, author_id, int),
							     ?Field(3, author_login, string), 
							     ?Field(4, title, string),
							     ?Field(5, send_date, datetime),
							     ?Field(6, url, string),
							     ?Field(7, items, #list{type = moodstrip_item_result}),
							     ?FieldOptional(8, count, int)
							    ]),
			      ?Struct(13, moodstrip_item_result, [?Field(1, url, string), ?Field(2, author_login, string), 
								  ?Field(3, send_date, datetime)]),
			      ?Struct(14, user_search_result, [?Field(1, page_number, int), ?Field(2, has_more, bool), 
							       ?Field(3, items, #list{type = user_info})]),
			      ?Struct(15, moodstrip_search_result, [?Field(1, page_number, int), ?Field(2, has_more, bool), 
								    ?Field(3, items, #list{type = moodstrip_result})]),
			      ?Struct(16, auth_ticket_result, [?Field(1, auth_ticket, binary), ?Field(2, user_id, int), 
							       ?Field(3, lifetime, int), ?Field(4, result_code, auth_ticket_result_code),
							       ?Field(5, lock_time, int)]),

			      ?Struct(17, notification_registration_result, [?Field(1, server, string), ?Field(2, key, string)]),
			      ?Struct(18, notification_result, [?Field(1, packet_id, int, ?CppType(int64)),
								?Field(2, notifications, #list{type = notification})]),
			      ?Struct(19, notification, [?Field(1, user_id, int), ?Field(2, event, event)]),
			      
			      ?Struct(20, authorization, [?Field(1, state, authorization_state), ?Field(2, message, string)]),
			      
			      ?Struct(21, send_message_result, [?Field(1, result_code, contact_result_code), ?Field(2, message_id, int),
								?Field(3,  send_date, datetime)]),
			      ?Struct(23, contact_logins_result, [?Field(1, result_code, standart_result_code),
								  ?Field(2, users, #list{type = contact_login}),
								  ?Field(3, channels, #list{type = contact_login})]),
			      ?Struct(30, published_images_result, [?Field(1, result_code, standart_result_code),
								    ?Field(2, images, #list{type = published_image})]),

			      % channel's results
			      ?Struct(25, channel_result, [?Field(1, channel_id, int),
							   ?Field(2, author_id, int),
							   ?Field(3, author_login, string),
							   ?Field(4, creation_date, datetime, ?CppType(date)),
							   ?Field(5, title, string),
							   ?Field(6, short_description, string),
							   ?Field(7, full_description, string),
							   ?Field(8, user_count, int),
							   ?Field(9, logo_url, string),
							   ?Field(10, moderators, #list{type = contact_login})
							  ]),
			      ?Struct(27, channel_search_result, [?Field(1, page_number, int), ?Field(2, has_more, bool),
								  ?Field(3, items, #list{type = channel_result})]),
			      ?Struct(29, channel_message, [?Field(1, result_code, artmessage_result_code),
							    ?Field(2, message_id, int), ?Field(3, author_id, int), ?Field(4, author_login, string),
							    ?Field(5, send_date, datetime), ?Field(6, data, binary), ?Field(7, metadata, string)
							   ]),
			      ?Struct(31, channel_message_url, [?Field(1, result_code, artmessage_result_code),
								?Field(2, message_id, int), ?Field(3, author_id, int), ?Field(4, author_login, string),
								?Field(5, send_date, datetime), ?Field(6, url, string)
							       ]),
			      ?Struct(32, command_package, [?Field(1, package_id, int), ?Field(2, items, #list{type = package_union})]),
			      ?Struct(33, package_union, [?Field(1, value, #union{types = [delete_message_command]})]),
			      ?Struct(34, delete_message_command, [?Field(1, contact_id, int), ?Field(2, message_id, int)]),

			      % last id = 35


			      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			      % client functions
			      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

			      % server info
			      ?Function(10093, get_server_info, [], server_info),

			      % special commands from server (delete message and so on)
			      ?Function(10049, get_commands, [?Field(1, previous_package_id, int)], command_package),

			      % auth
			      ?Function(10001,  get_auth_ticket, [?Field(1, login, string), ?Field(2, password, string)], auth_ticket_result),
			      ?Function(10003,  create_account, [?Field(1, user_account, user_account), ?FieldOptional(2, invite_code, string)],
					account_result_code),

			      % account
			      ?Function(10039, get_user_info, [?Field(1, user_id, int)], user_info), % for web also
			      ?Function(10097, get_user_info_by_login, [?Field(1, login, string)], user_info),
			      ?Function(10073, get_my_account, [], user_account),
			      ?Function(10005, update_account, [?Field(1, user_account, user_account), ?Field(2, has_user_picture, bool),
								?FieldOptional(3, user_picture, binary), 
								?FieldDefault(4, content_type, string, "image/png")],
					account_result_code),
			      ?Function(10009,  get_user_picture, [?Field(1, user_id, int), 
								   ?FieldOptional(2, last_changed_date, datetime)], user_picture_result),
			      ?Function(10037, update_password, [?Field(1, new_password, string), ?Field(2, old_password, string)], 
					account_result_code),
			      ?Function(10063, reset_password, [?Field(1, login, string)], account_result_code),

			      % contacts
			      ?Function(10011, get_contacts, [], #list{type = contact_info}),
			      ?Function(10081, get_contact, [?Field(1, user_id, int)], contact_info),
			      ?Function(10083, get_status, [?Field(1, user_id, int)], user_status),
			      ?Function(10085, get_authorization, [?Field(1, user_id, int)], authorization),
			      ?Function(10015, process_authorization_request, [?Field(1, recipient_id, int), ?Field(2, authorization_message, string)], 
					authorization_result_code),
			      ?Function(10099, process_authorization_request_by_login, [?Field(1, recipient_login, string), 
											?Field(2, authorization_message, string)], 
					authorization_result_code),
			      ?Function(10017, process_authorization_response, [?Field(1, recipient_id, int), ?Field(2, is_accepted, bool)], 
					authorization_result_code),
			      ?Function(10019, remove_from_contacts, [?Field(1, contact_user_id, int)], contact_result_code),
			      ?Function(10029, block_contact, [?Field(1, contact_user_id, int)], contact_result_code),
			      ?Function(10031, unblock_contact, [?Field(1, contact_user_id, int)], contact_result_code),
			      ?Function(10033, simple_search_contacts, [?Field(1, page_number, int), % also for web
									 ?Field(2, records_per_page, int), 
									 ?Field(3, value, string)], user_search_result),
			      ?Function(10035, advanced_search_contacts, [?Field(1, page_number, int), % also for web
									  ?Field(2, records_per_page, int), 
									  ?Field(3, value, string),
									  ?Field(4, country, country),
									  ?Field(5, city, string),
									  ?Field(6, sex, sex),
									  ?Field(7, min_age, int),
									  ?Field(8, max_age, int)], user_search_result),

			      % messages
			      ?Function(10021, send_friend_message, [?Field(1, is_public, bool), ?Field(2, message, binary), 
								     ?FieldOptional(3, metadata, string)], send_message_result),
			      ?Function(10023, send_private_message, [?Field(1, recipient_id, int), ?Field(2, message, binary), 
								      ?FieldOptional(3, metadata, string)], send_message_result),
			      ?Function(10113, get_next_artmessage_and_report, [?Field(1, previous_message_id, int)], art_message),
			      
			      % moodstrips & publishing
			      ?Function(10045, create_moodstrip, [?Field(1, caption, string), ?FieldOptional(2, is_hidden, bool),
								  ?FieldOptional(3, channel_id, int)], int),
			      ?Function(10071, add_picture_to_moodstrip, [?Field(1, moodstrip_id, int),
									  ?Field(2, message_id, int),
									  ?Field(3, author, string),
									  ?Field(4, data, binary),
									  ?FieldDefault(5, content_type, string, "image/png"),
									  ?FieldOptional(6, is_last, bool)
								       ], 
					publishing_moodstrip_result),
			      ?Function(10051, delete_moodstrip, [?Field(1, moodstrip_id, int)], moodstrip_result_code),

			      % notification
			      ?Function(10075, notification_register, [], notification_registration_result),
			      ?Function(10077, notification_unregister, [?Field(1, key, string)], ok_result_code),
			      ?Function(10079, get_notifications, [?Field(1, key, string), ?FieldOptional(2, packet_id, int, ?CppType(int64))],
					notification_result),
			      ?Function(10089, get_notification_timeout, [], int),

			      % channels
			      ?Function(10101, search_channel, [?Field(1, page_number, int), ?Field(2, records_per_page, int), 
								?Field(3, value, string)], channel_search_result),
			      ?Function(10103, get_channel_info, [?Field(1, channel_id, int)], channel_result),
			      ?Function(10105, send_channel_message, [?Field(1, channel_id, int), ?Field(2, message, binary), 
								      ?FieldOptional(3, metadata, string)], send_message_result),
			      ?Function(10107, get_next_channel_message, [?Field(1, channel_id, int), ?Field(2, last_message_id, int),
									  ?FieldOptional(3, skip_message, bool)],
					channel_message),
			      ?Function(10007, get_next_channel_message_url, [?Field(1, channel_id, int), ?Field(2, last_message_id, int),
									      ?Field(3, skip_message, bool)],
					channel_message_url),
			      ?Function(10109, add_user_to_channel, [?Field(1, channel_id, int)], change_user_channel_result),
			      ?Function(10111, delete_user_from_channel, [?Field(1, channel_id, int)], change_user_channel_result),
			      ?Function(10027, update_channel, [?Field(1, channel_id, int), ?Field(2, title, string),
							        ?Field(3, short_description, string), ?Field(4, full_description, string),
							        ?Field(5, order, int)],
					standart_result_code, #cpp_class_options{skip = true}),
			      ?Function(10025, obscene_channel_message, [?Field(1, channel_id, int), ?Field(2, message_id, int)],
					standart_result_code),
			      ?Function(10047, delete_message, [?Field(1, contact_id, int), ?Field(2, message_type, message_type),
								?Field(3, message_id, int)], standart_result_code),
			       			      
			      % invitation
			      ?Function(10091, check_invitation, [?Field(1, code, string)], invitation_result_code),
			      
			      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			      % web only functions
			      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

			      % user info
			      ?Function(10061, get_sorted_users, [?Field(1, page_number, int), 
							           ?Field(2, records_per_page, int), 
								   ?Field(3, sorted_by, sorted_by),
                                                                   ?Field(4, date_range, date_range)
                                                                  ], user_search_result, #cpp_class_options{skip = true}),
			      ?Function(10043, get_contact_logins, [?Field(1, contact_type, contact_type), ?Field(2, contact_id, int)],
					contact_logins_result, #cpp_class_options{skip = true}),

			      % moodstrips & publish
			      ?Function(10053, get_moodstrip, [?Field(1, moodstrip_id, int), ?FieldDefault(2, is_preview, bool, false)], 
					moodstrip_result, #cpp_class_options{skip = true}),
			      ?Function(10055, search_moodstrip, [?Field(1, page_number, int), ?Field(2, records_per_page, int), 
								   ?Field(3, value, string)], 
					moodstrip_search_result, #cpp_class_options{skip = true}),
			      ?Function(10057, get_last_moodstrips, [?Field(1, count, int)], #list{type = moodstrip_result}, 
					#cpp_class_options{skip = true}),
			      ?Function(10059, get_sorted_moodstrips, [?Field(1, page_number, int), 
								       ?Field(2, records_per_page, int), 
								       ?Field(3, sorted_by, sorted_by),
								       ?Field(4, date_range, date_range)
                                                                       ], moodstrip_search_result, #cpp_class_options{skip = true}),
			      ?Function(10069, get_contact_moodstrips, [?Field(1, page_number, int),
									?Field(2, records_per_page, int),
									?Field(3, contact_type, contact_type),
									?Field(4, contact_id, int)], moodstrip_search_result,
					#cpp_class_options{skip = true}),
			      ?Function(10113, get_contact_moodstrips_4rss, [?Field(1, contact_type, contact_type),
									     ?Field(2, contact_id, int), 
									     ?Field(3, count, int)], #list{type = moodstrip_result},
					#cpp_class_options{skip = true}),
			      ?Function(10041, get_published_images, [?Field(1, contact_type, contact_type), ?Field(2, contact_id, int)], 
					published_images_result, #cpp_class_options{skip = true}),

			      % password reset & unsubscribe
			      ?Function(10065, get_recovered_password, [?Field(1, user_id, int), 
									?Field(2, reset_code, string)], string, 
					#cpp_class_options{skip = true}),
			      ?Function(10067, unsubscribe, [?Field(1, user_id, int), 
							     ?Field(2, unsubscribe_code, string)], unsubscribe_result_code, 
					#cpp_class_options{skip = true}),
			      ?Function(10095, send_email, [?Field(1, moodstrip_id, int), ?Field(2, text, string)], standart_result_code, 
					#cpp_class_options{skip = true}),
			      
			      			      
			      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			      % server only functions
			      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

			      ?Function(10087, get_srv_address, [], string, #cpp_class_options{skip = true}),

			      % last id = 10115


			      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			      % enums
			      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			      			      			      
			      ?Enum(20020, version_tag, [{undefined, 0}, {api, 1}, {desktop_client, 2}, {web_site, 3}, {web_widget, 4}, 
							 {mobile_client, 5}]),
			      ?Enum(20021, version_support, [{undefined, 0}, {supported, 1}, {not_supported, 2}]),
			      ?Enum(20001, sex, [{undefined, 0}, {male, 1}, {female, 2}]),
			      ?Enum(20002, country, moodbox_country:get_countries(),
				    #cpp_class_options{replace_with = "QLocale::Country", include_lib = "QLocale", default = "QLocale::AnyCountry"}),
			      ?Enum(20003, language, [{undefined, 0}, {en, 1}, {ru, 2}]),
			      ?Enum(20007, message_type, [{undefined, 0}, {?friends, 1}, {?private, 2}, {?group, 3}, {?channel, 4}]),
			      ?Enum(20008, authorization_state, [{undefined, 0}, {?authorized, 1}, {?waits_authorization_from_me, 2}, 
								 {?not_authorized_me, 3}]),
			      ?Enum(20012, date_range, [{undefined, 0}, {?today, 1}, {?week, 2}, {?month, 3}, {?all_time, 10}]),
			      ?Enum(20013, sorted_by, [{undefined, 0}, {?most_recent, 1}]),
			      ?Enum(20015, user_status, [{undefined, 0}, {?offline, 1}, {?online, 2}, 
							 {?connecting, 100}]), % 'connecting' is used client-side only
			      ?Enum(20023, contact_type, [{undefined, 0}, {?friend, 1}, {?channel, 2}]),

			      % result codes
			      ?Enum(20004, ok_result_code, [{undefined, 0}, {ok, 1}]),
			      ?Enum(20017, auth_ticket_result_code, [{ok, 1}, {invalid_credentials, 2}, {locked_too_many_attempts, 3}]),
			      ?Enum(20005, account_result_code, [{ok, 1}, {?invalid_login, 2}, {?login_is_not_available, 3}, {?invalid_password, 4}, 
								 {?invalid_email, 5}, {?invalid_invite_code, 6}, {?userpic_size_too_big, 7}, 
								 {?account_not_found, 8}, {?age_must_be_13_or_greater, 9}]),
			      ?Enum(20010, user_picture_result_code, [{ok, 1}, {?userpic_size_too_big, 2}, {?not_modified, 3}, {?not_found, 4}]),
			      ?Enum(20006, authorization_result_code, [{ok, 1}, {?incorrect_db_data, 2}, {?author_has_too_many_contacts, 3}, 
								       {?recipient_has_too_many_contacts, 4}, {?already_authorized, 5}, 
								       {?not_waiting_for_authorization, 6}, {?authorized_by_this_request, 7},
								       {?account_not_found, 8}]),
			      ?Enum(20009, contact_result_code, [{ok, 1}, {?contact_not_found, 2}, {?not_authorized_me, 3}, {?closed_contact, 4}]),
			      ?Enum(20011, moodstrip_result_code, [{ok, 1}, {?moodstrip_not_found, 2}, {?moodstrip_is_published, 3}, 
								   {?moodstrip_is_empty, 4}]),
			      ?Enum(20014, unsubscribe_result_code, [{undefined, 0}, {ok, 1}, {?unsubscribe_error, 2}]),
			      ?Enum(20016, event, [{?status_changed, 1}, {?userpic_changed, 2}, {?contact_changed, 3}, {?authorization_changed, 4}, 
						   {?new_message, 5}, {?disconnect, 6}, {?reload, 7}, {?new_channel_message, 8}, 
						   {?new_command, 9}]),
			      ?Enum(20018, artmessage_result_code, [{ok, 1}, {?message_data_not_found, 2}]),
			      ?Enum(20019, invitation_result_code, [{ok, 1}, {?invalid_invite_code, 2}, {?limit_was_exceeded, 3}, {?invalid_email, 4}]),
			      ?Enum(20022, standart_result_code, [{undefined, 0}, {ok, 1}, {?empty, 2}, {?hidden, 3}, {?forbidden, 4}]),
			      ?Enum(20025, change_user_channel_result, [{undefined, 0}, {ok, 1}]),
			      ?Enum(20026, url_code, [{undefined, 0}, {?simple_url, 1}, {?embedded_flash, 2}, {?lj, 3}, {?lj_cut, 4}]),
			      ?Enum(20027, role, [{undefined, 0}, {?admin, 1}])
			      			      	      			      
			      % last id = 20027
			     ]
	       }.
