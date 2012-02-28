-module(moodbox_database).

-export([create/0, create/1, create/2, delete/0, delete_old_records/2, delete_channel_messages/1, get_subscribers/0]).
-export([update_account/2, update_password/3, get_user_info/1, get_user_info_by_login/1, get_my_account/2, get_contacts/1, 
	 get_contacts/2, get_status/2, get_authorization/2, get_contact_logins/2, remove_from_contacts/2, simple_search_contacts/3, 
	 advanced_search_contacts/8, send_friend_message/3, send_private_message/3, delivery_artmessage/3, get_next_artmessage_and_report/2,
	 skip_artmessages/2, process_authorization_request/4, process_authorization_request_by_login/4, process_authorization_response/3, 
	 logon/6, block_contact/2, unblock_contact/2, get_published_images/2, get_sorted_users/4, reset_password/3, get_recovered_password/2, 
	 unsubscribe/2, send_email/2]).
% command functions
-export([get_commands/2]).
% moodstrips functions
-export([create_moodstrip/6, add_image_to_moodstrip/7, mark_moodstrip_for_deleting/2, delete_moodstrip/1, get_moodstrip/2, 
	 search_moodstrip/4, get_last_moodstrips/3, get_sorted_moodstrips/5, get_contact_moodstrips/4, hide_moodstrip/2, 
	 get_contact_moodstrips_4rss/3]).
% notification functions
-export([notify/3, notify_by_user/2, notify_by_channel/3, notification_register/2, notification_unregister/2, add_notification_server/2, 
	 check_notification_server/1, delete_notification_server/1, get_notification_server/0, check_notification_key/2, add_notification_error/2]).
% channel functions
-export([search_channel/4, get_channel_info/1, send_channel_message/3, delivery_channel_message/4, get_next_channel_message/3, add_user_to_channel/2, 
	 delete_user_from_channel/2, update_channel/7, channel_user_blocking_status/3, obscene_channel_message/3, change_moderator_channel_list/3,
	 delete_message/4]).

-import(mnesia).
-import(moodbox_sequence).

-include("moodbox_config.hrl").
-include("moodbox_facade.hrl").
-include("moodbox_database.hrl").
-include("moodbox_repository.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(SamplePicturesPath, "priv").
-define(datetime, moodbox_datetime).
-define(repository, moodbox_repository).
-define(sequence, moodbox_sequence).
-define(invitation, moodbox_invitation).
-define(string, moodbox_string).

%% delivery state
-define(saving, saving).
-define(to_be_delivered, to_be_delivered).
-define(delivered, delivered).
-define(channel_delivered, channel_delivered).

%% internal records
-record(logon_info, {user_id, lock_time, failed_attempts_count, last_failed_attempt_time}).
-record(user_password_reset_request, {user_id, request_date, new_password, reset_code}).

%%% exceptions
-export([get_exception_message/1]).
-include("moodbox_exception.hrl").
-record(cannot_select_notification_server, {}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creation of DB

create() ->
    create([node()], main).

create(Nodes) ->
    create(Nodes, main).

create(Nodes, Mode) ->
  mnesia:stop(),
  mnesia:create_schema(Nodes),
  mnesia:start(),
  try
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Notification tables %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    mnesia:create_table(notification_server,
			[{ram_copies, Nodes},
			 {attributes, record_info(fields, notification_server)}]),
    mnesia:create_table(session,
			[{ram_copies, Nodes},
			 {frag_properties, [{node_pool, Nodes}, {n_ram_copies, 1}]},
			 {attributes, record_info(fields, session)}]),
    mnesia:create_table(notification_error,
			[{ram_copies, Nodes},
			 {index, [server_pid]},
			 {attributes, record_info(fields, notification_error)}]),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Invitations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    mnesia:create_table(invitation_prefix,
			[{disc_copies, Nodes},
			 {frag_properties, [{node_pool, Nodes}, {n_disc_copies, 1}]},
			 {attributes, record_info(fields, invitation_prefix)}]),
    mnesia:create_table(invitation,
			[{disc_copies, Nodes},
			 {index, [code]},
			 {frag_properties, [{node_pool, Nodes}, {n_disc_copies, 1}]},
			 {attributes, record_info(fields, invitation)}]),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Main tables %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    mnesia:create_table(user_account_data,
			[{disc_copies, Nodes},
			 {index, [login, upper_login]},
			 {frag_properties, [{node_pool, Nodes}, {n_disc_copies, 1}]},
			 {attributes, record_info(fields, user_account_data)}]),
    mnesia:create_table(user_account_sorted,
			[{disc_copies, Nodes},
			 {type, ordered_set},
			 {attributes, record_info(fields, user_account_sorted)}]),
    mnesia:create_table(art_message,
			[{disc_copies, Nodes},
			 {frag_properties, [{node_pool, Nodes}, {n_disc_copies, 1}]},
			 {attributes, record_info(fields, art_message)}]),
    mnesia:create_table(published_message,
			[{disc_copies, Nodes},
			 {frag_properties, [{node_pool, Nodes}, {n_disc_copies, 1}]},
			 {attributes, record_info(fields, published_message)}]),
    mnesia:create_table(delivery,
			[{disc_copies, Nodes},
			 {frag_properties, [{node_pool, Nodes}, {n_disc_copies, 1}]},
			 {attributes, record_info(fields, delivery)}]),
    mnesia:create_table(contact,
			[{disc_copies, Nodes},
			 {frag_properties, [{node_pool, Nodes}, {n_disc_copies, 1}]},
			 {attributes, record_info(fields, contact)}]),
    mnesia:create_table(logon_info,
			[{disc_copies, Nodes},
			 {frag_properties, [{node_pool, Nodes}, {n_disc_copies, 1}]},
			 {attributes, record_info(fields, logon_info)}]),
    mnesia:create_table(user_password_reset_request,
			[{disc_copies, Nodes},
			 {frag_properties, [{node_pool, Nodes}, {n_disc_copies, 1}]},
			 {attributes, record_info(fields, user_password_reset_request)}]),
    mnesia:create_table(moodstrip,
			[{disc_copies, Nodes},
			 {index, [author_id, channel_id]},
			 {frag_properties, [{node_pool, Nodes}, {n_disc_copies, 1}]},
			 {attributes, record_info(fields, moodstrip)}]),
    mnesia:create_table(moodstrip_sorted,
			[{disc_copies, Nodes},
			 {type, ordered_set},
			 {attributes, record_info(fields, moodstrip_sorted)}]),
    mnesia:create_table(moodstrip_sorted_ru,
			[{disc_copies, Nodes},
			 {type, ordered_set},
			 {attributes, record_info(fields, moodstrip_sorted_ru)}]),
    mnesia:create_table(channel,
			[{disc_copies, Nodes},
			 {frag_properties, [{node_pool, Nodes}, {n_disc_copies, 1}]},
			 {attributes, record_info(fields, channel)}]),
    mnesia:create_table(channel_message,
			[{disc_copies, Nodes},
			 {frag_properties, [{node_pool, Nodes}, {n_disc_copies, 1}]},
			 {attributes, record_info(fields, channel_message)}]),
    mnesia:create_table(channel_moderator,
			[{disc_copies, Nodes},
			 {frag_properties, [{node_pool, Nodes}, {n_disc_copies, 1}]},
			 {attributes, record_info(fields, channel_moderator)}]),
    ?sequence:create_sequence(Nodes),
    if 
	Mode == test ->
	    %% create test data
	    crypto:start(),
	    update_account(#user_account{id = 0, login = "arty", password = "test1", name = "Arty Dude", email = "vostrjakov@mail.ru", 
					 birth_day = ?datetime:from_age(32, true)}),
	    update_account(#user_account{id = 0, login = "test1", password = "test1", name = "test1", email = "vostrjakov@mail.ru", city = "Moscow"}),
	    update_account(#user_account{id = 0, login = "test2", password = "test2", sex = male, name = "test2", birth_day = ?datetime:from_age(2, true), 
					 country = 'RU'}),
	    update_account(#user_account{id = 0, login = "test3", password = "test3", sex = female, name = "test3", 
					 birth_day = ?datetime:from_age(3, true), 
					 country = 'RU'}),
	    update_account(#user_account{id = 0, login = "test4", password = "test4", name = "test4", sex = female, 
					 birth_day = ?datetime:from_age(4, true), country = 'RU'}),
	    update_account(#user_account{id = 0, login = "test5", password = "test5", name = "test5", sex = female, 
					 birth_day = ?datetime:from_age(5, true), country = 'RU'}),
	    update_account(#user_account{id = 0, login = "test6", password = "test6", name = "test6", birth_day = ?datetime:from_age(6, true), 
					 country = 'RU'}),
	    update_account(#user_account{id = 0, login = "test7", password = "test7", name = "test7", birth_day = ?datetime:from_age(7, true), 
					 country = 'RU'}),
	    update_account(#user_account{id = 0, login = "test8", password = "test8", name = "test8", birth_day = ?datetime:from_age(8, true), 
					 country = 'RU'}),
	    update_account(#user_account{id = 0, login = "test9", password = "test9", name = "test9", birth_day = ?datetime:from_age(9, true), 
					 country = 'RU'}),
	    update_account(#user_account{id = 0, login = "test10", password = "test10", name = "test10", birth_day = ?datetime:from_age(10, true), 
					 country = 'RU'}),
	    % add several friends in arty's contact list
	    process_authorization_request(1, 2, "Hi, Dude", 100),
	    process_authorization_response(2, 1, true),
	    process_authorization_request(1, 3, "Hi, Dude", 100),
	    process_authorization_response(3, 1, true),
	    process_authorization_request(1, 4, "Hi, Dude", 100),
	    process_authorization_response(4, 1, true),
	    process_authorization_request(1, 5, "Hi, Dude", 100),
	    process_authorization_response(5, 1, true),

	    process_authorization_request(2, 3, "Hi, Dude", 100),
	    process_authorization_response(3, 2, true),
	    process_authorization_request(2, 4, "Hi, Dude", 100),
	    process_authorization_response(4, 2, true),
	    process_authorization_request(2, 5, "Hi, Dude", 100),
	    process_authorization_response(5, 2, true),
	    process_authorization_request(2, 6, "Hi, Dude", 100),
	    process_authorization_response(6, 2, true),
	    process_authorization_request(2, 7, "Hi, Dude", 100),
	    process_authorization_response(7, 2, true),
	    process_authorization_request(2, 8, "Hi, Dude", 100),
	    process_authorization_response(8, 2, true),

	    process_authorization_request(3, 4, "Hi, Dude", 100),
	    process_authorization_response(4, 3, true),
	    process_authorization_request(3, 5, "Hi, Dude", 100),
	    process_authorization_response(5, 3, true),
	    process_authorization_request(3, 6, "Hi, Dude", 100),
	    process_authorization_response(6, 3, true),
	    process_authorization_request(3, 6, "Hi, Dude", 100),
	    process_authorization_response(7, 3, true),
	    process_authorization_request(3, 7, "Hi, Dude", 100),
	    process_authorization_response(8, 3, true),
	    process_authorization_request(3, 8, "Hi, Dude", 100),
	    process_authorization_response(9, 3, true),

	    process_authorization_request(4, 5, "Hi, Dude", 100),
	    process_authorization_response(5, 4, true),
	    process_authorization_request(4, 6, "Hi, Dude", 100),
	    process_authorization_response(6, 4, true),
	    process_authorization_request(4, 7, "Hi, Dude", 100),
	    process_authorization_response(7, 4, true),
	    process_authorization_request(4, 8, "Hi, Dude", 100),
	    process_authorization_response(8, 4, true),
	    process_authorization_request(4, 9, "Hi, Dude", 100),
	    process_authorization_response(9, 4, true),
	    process_authorization_request(4, 10, "Hi, Dude", 100),
	    process_authorization_response(10, 4, true),

	    process_authorization_request(5, 6, "Hi, Dude", 100),
	    process_authorization_response(6, 5, true),
	    process_authorization_request(5, 7, "Hi, Dude", 100),
	    process_authorization_response(7, 5, true),
	    process_authorization_request(5, 8, "Hi, Dude", 100),
	    process_authorization_response(8, 5, true),
	    process_authorization_request(5, 9, "Hi, Dude", 100),
	    process_authorization_response(9, 5, true),
	    process_authorization_request(5, 10, "Hi, Dude", 100),
	    process_authorization_response(10, 5, true),
	    process_authorization_request(5, 11, "Hi, Dude", 100),
	    process_authorization_response(11, 5, true),

	    %% create moodstrip and published image of arty (for web-site tests)
	    MoodstripId = ?sequence:sequence(moodstrip),
	    mnesia:transaction(fun() -> mnesia:write(#moodstrip{id = MoodstripId, author_id = 1, author_login = "arty",
								send_date = ?datetime:now(), title = "Test MoodStrip", is_published = true,
								items = [#moodstrip_item{image_id = 1, author_login = "arty", 
											 send_date = ?datetime:now()},
									 #moodstrip_item{image_id = 2, author_login = "arty", 
											 send_date = ?datetime:now()},
									 #moodstrip_item{image_id = 3, author_login = "arty", 
											 send_date = ?datetime:now()},
									 #moodstrip_item{image_id = 4, author_login = "arty", 
											 send_date = ?datetime:now()},
									 #moodstrip_item{image_id = 5, author_login = "arty", 
											 send_date = ?datetime:now()},
									 #moodstrip_item{image_id = 6, author_login = "arty", 
											 send_date = ?datetime:now()},
									 #moodstrip_item{image_id = 7, author_login = "arty", 
											 send_date = ?datetime:now()},
									 #moodstrip_item{image_id = 8, author_login = "arty", 
											 send_date = ?datetime:now()},
									 #moodstrip_item{image_id = 9, author_login = "arty", 
											 send_date = ?datetime:now()},
									 #moodstrip_item{image_id = 10, author_login = "test2", 
											 send_date = ?datetime:now()},
									 #moodstrip_item{image_id = 1, author_login = "test1", 
											 send_date = ?datetime:now()},
									 #moodstrip_item{image_id = 2, author_login = "test2", 
											 send_date = ?datetime:now()},
									 #moodstrip_item{image_id = 3, author_login = "test3", 
											 send_date = ?datetime:now()},
									 #moodstrip_item{image_id = 4, author_login = "test4", 
											 send_date = ?datetime:now()},
									 #moodstrip_item{image_id = 5, author_login = "test5", 
											 send_date = ?datetime:now()}]}),
					mnesia:write(#moodstrip_sorted{creation_date = ?datetime:now(), id = MoodstripId}),
	    MoodstripId1 = ?sequence:sequence(moodstrip),
	    mnesia:write(#moodstrip{id = MoodstripId1, author_id = 1, author_login = "arty",
				    send_date = ?datetime:now()+100000000000, title = "Test MoodStrip 2", is_published = true,
				    items = [#moodstrip_item{image_id = 2, author_login = "test1", send_date = ?datetime:now()}]}),
	    mnesia:write(#moodstrip_sorted{creation_date = ?datetime:now()+100000000000, id = MoodstripId1}),
	    MoodstripId2 = ?sequence:sequence(moodstrip),
	    mnesia:write(#moodstrip{id = MoodstripId2, author_id = 1, author_login = "arty",
				    send_date = ?datetime:now()+200000000000, title = "Test MoodStrip 3", is_published = true,
				    items = [#moodstrip_item{image_id = 3, author_login = "test3", send_date = ?datetime:now()}]}),
	    mnesia:write(#moodstrip_sorted{creation_date = ?datetime:now()+200000000000, id = MoodstripId2}),
            MoodstripId3 = ?sequence:sequence(moodstrip),
	    mnesia:write(#moodstrip{id = MoodstripId3, author_id = 1, author_login = "arty",
				    send_date = ?datetime:now()+300000000000, title = "Test MoodStrip 4", is_published = true,
				    items = [#moodstrip_item{image_id = 4, author_login = "test4", send_date = ?datetime:now()}]}),
            mnesia:write(#moodstrip_sorted{creation_date = ?datetime:now()+300000000000, id = MoodstripId3}),
            MoodstripId4 = ?sequence:sequence(moodstrip),
	    mnesia:write(#moodstrip{id = MoodstripId4, author_id = 1, author_login = "arty",
				    send_date = ?datetime:now()+400000000000, title = "Test MoodStrip 5", is_published = true,
				    items = [#moodstrip_item{image_id = 5, author_login = "test5", send_date = ?datetime:now()}]}),
            mnesia:write(#moodstrip_sorted{creation_date = ?datetime:now()+400000000000, id = MoodstripId4}),
            MoodstripId5 = ?sequence:sequence(moodstrip),
	    mnesia:write(#moodstrip{id = MoodstripId5, author_id = 1, author_login = "arty",
				    send_date = ?datetime:now()+500000000000, title = "Test MoodStrip 6", is_published = true,
				    items = [#moodstrip_item{image_id = 6, author_login = "test6", send_date = ?datetime:now()}]}),
            mnesia:write(#moodstrip_sorted{creation_date = ?datetime:now()+500000000000, id = MoodstripId5}),
            MoodstripId6 = ?sequence:sequence(moodstrip),
	    mnesia:write(#moodstrip{id = MoodstripId6, author_id = 1, author_login = "arty",
				    send_date = ?datetime:now()+600000000000, title = "Test MoodStrip 7", is_published = true,
				    items = [#moodstrip_item{image_id = 7, author_login = "test7", send_date = ?datetime:now()}]}),
            mnesia:write(#moodstrip_sorted{creation_date = ?datetime:now()+600000000000, id = MoodstripId6}),
            MoodstripId7 = ?sequence:sequence(moodstrip),
	    mnesia:write(#moodstrip{id = MoodstripId7, author_id = 1, author_login = "arty",
				    send_date = ?datetime:now()+700000000000, title = "Test MoodStrip 8", is_published = true,
				    items = [#moodstrip_item{image_id = 8, author_login = "test8", send_date = ?datetime:now()}]}),
            mnesia:write(#moodstrip_sorted{creation_date = ?datetime:now()+700000000000, id = MoodstripId7}),
            MoodstripId8 = ?sequence:sequence(moodstrip),
	    mnesia:write(#moodstrip{id = MoodstripId8, author_id = 1, author_login = "arty",
				    send_date = ?datetime:now()+800000000000, title = "Test MoodStrip 9", is_published = true,
				    items = [#moodstrip_item{image_id = 9, author_login = "test9", send_date = ?datetime:now()}]}),
            mnesia:write(#moodstrip_sorted{creation_date = ?datetime:now()+800000000000, id = MoodstripId8}),
            MoodstripId9 = ?sequence:sequence(moodstrip),
	    mnesia:write(#moodstrip{id = MoodstripId9, author_id = 1, author_login = "arty",
				    send_date = ?datetime:now()+900000000000, title = "Test MoodStrip 10", is_published = true,
				    items = [#moodstrip_item{image_id = 10, author_login = "test10", send_date = ?datetime:now()}]}),
            mnesia:write(#moodstrip_sorted{creation_date = ?datetime:now()+900000000000, id = MoodstripId9})
	    end),
	    ?sequence:sequence(?MOODSTRIP_ITEM_COUNTER, 100),
	    ?sequence:sequence(art_message, 100);
	true ->
	    ok
    end
  catch 
      _Class:Exception ->
	  error_logger:error_report([{type, create_database}, {exception, Exception}, {trace, erlang:get_stacktrace()}])
  end.
    
delete() ->
    mnesia:delete_table(notification_server),
    mnesia:delete_table(session),
    mnesia:delete_table(notification_error),
    mnesia:delete_table(invitation_prefix),
    mnesia:delete_table(invitation),
    mnesia:delete_table(user_account_data),
    mnesia:delete_table(user_account_sorted),
    mnesia:delete_table(art_message),
    mnesia:delete_table(published_message),
    mnesia:delete_table(delivery),
    mnesia:delete_table(contact),
    mnesia:delete_table(logon_info),
    mnesia:delete_table(user_password_reset_request),
    mnesia:delete_table(sequence),
    mnesia:delete_table(moodstrip),
    mnesia:delete_table(moodstrip_sorted),
    mnesia:delete_table(moodstrip_sorted_ru),
    mnesia:delete_table(channel),
    mnesia:delete_table(channel_message),
    mnesia:delete_table(channel_moderator).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DB API

update_account(User) ->
    update_account(User, undefined).

update_account(User, Language) ->
    F =  fun() -> 
		 if 
		     User#user_account.id == 0 ->
			 %% check if this login already exists
			 UpperLogin = string:to_upper(User#user_account.login),
			 case mnesia:index_read(user_account_data, UpperLogin, #user_account_data.upper_login) of
			     [] ->
				 UserId = ?sequence:sequence(user),
				 NewUser = #user_account_data{id = UserId,
							      login = User#user_account.login,
							      upper_login = UpperLogin,
							      password = crypto:rc4_encrypt(xmerl_ucs:to_utf8(User#user_account.password), 
											    xmerl_ucs:to_utf8(User#user_account.password)),
							      creation_date = ?datetime:now(),
							      is_logon_allowed = true,
							      motto = User#user_account.motto,
							      about_me = User#user_account.about_me, 
							      name = User#user_account.name, 
							      upper_name = ?string:to_upper(User#user_account.name),
							      country = User#user_account.country,
							      city = User#user_account.city, 
							      email = User#user_account.email,
							      upper_email = ?string:to_upper(User#user_account.email),
							      sex = User#user_account.sex,
							      birth_day = User#user_account.birth_day,
							      language = Language,
							      allow_news = User#user_account.allow_news,
							      allow_publishing = User#user_account.allow_publishing,
							      allow_show_friends = User#user_account.allow_show_friends,
							      role = undefined},
				 mnesia:write(NewUser),
				 mnesia:write(#user_account_sorted{creation_date = NewUser#user_account_data.creation_date, id = UserId}),
				 
				 % automaticaly add newbie channel to contact list
				 if 
				     Language == 'ru' ->
					 ok;
				     true ->
					 NewbieChannelId = ?NewbieChannelId,
					 case mnesia:read(channel, NewbieChannelId, write) of
					     [] ->
						 ok;
					     [NewbieChannel] ->
						 mnesia:write(#contact{owner_id = UserId, channels = [#contact_channel{channel_id = NewbieChannelId}]}),
						 mnesia:write(NewbieChannel#channel{users = [UserId | NewbieChannel#channel.users],
										    user_count = NewbieChannel#channel.user_count + 1})
					 end
				 end,
				 	 
				 % delete unnecessary records from sorted table
				 Count = mnesia:table_info(user_account_sorted, size),
				 if 
				     Count > ?MAX_SORTED_RECORDS_COUNT * 1.2 ->
					 spawn(?MODULE, delete_old_records, [user_account_sorted, ?MAX_SORTED_RECORDS_COUNT]);
				     true ->
					 ok
				 end,
				 ok;
			     _Users ->
				 ?login_is_not_available
			 end;
		     true ->
			 case mnesia:read(user_account_data, User#user_account.id, write) of
			     [] ->
				 ?account_not_found;
			     [CurrentUser] ->
				 UpdatedUser = CurrentUser#user_account_data{
				                     motto = User#user_account.motto,
						     about_me = User#user_account.about_me, 
						     name = User#user_account.name,
						     upper_name = ?string:to_upper(User#user_account.name),
						     country = User#user_account.country,
						     city = User#user_account.city, 
						     email = User#user_account.email,
						     upper_email = ?string:to_upper(User#user_account.email),
						     sex = User#user_account.sex,
                                                     birth_day = User#user_account.birth_day,
						     language = User#user_account.language, 
						     allow_news = User#user_account.allow_news,
						     allow_publishing = User#user_account.allow_publishing,
						     allow_show_friends = User#user_account.allow_show_friends},
				 mnesia:write(UpdatedUser),
				 % notification
				 if 
				     CurrentUser#user_account_data.name =/= User#user_account.name orelse 
				     CurrentUser#user_account_data.motto =/= User#user_account.motto ->
					 {ok, ?notify};
				     true ->
					 ok
				 end
			 end
		 end
	 end,
    mnesia:activity(transaction, F, [], mnesia_frag).

update_password(Id, NewPassword, CurrentPassword) ->
    F = fun() -> case mnesia:read(user_account_data, Id, write) of
		     [] -> 
			 ?account_not_found;
		     [User] -> 
			 CryptedPassword = crypto:rc4_encrypt(xmerl_ucs:to_utf8(CurrentPassword), xmerl_ucs:to_utf8(CurrentPassword)),
			 if 
			     User#user_account_data.password == CryptedPassword ->
				 mnesia:write(User#user_account_data{password = crypto:rc4_encrypt(xmerl_ucs:to_utf8(NewPassword), 
											      xmerl_ucs:to_utf8(NewPassword))});
			     true -> 
				 ?invalid_password
			 end
                 end
	end,
     mnesia:activity(transaction, F, [], mnesia_frag).

get_user_info(UserId) ->
    get_user_info_internal(UserId, undefined).

get_user_info_by_login(Login) ->
    get_user_info_internal(undefined, Login).

get_user_info_internal(UserId, Login) ->
    F = fun() -> 
		Result = case Login of
			     undefined ->
				 mnesia:read(user_account_data, UserId, read);
			     _ ->
				 mnesia:index_read(user_account_data, Login, #user_account_data.login)
			 end,
		case Result of
		    [] ->
			undefined;
		    [U] ->
			#user_info{user_id = U#user_account_data.id, login = U#user_account_data.login,
				   name = U#user_account_data.name, creation_date = U#user_account_data.creation_date, 
				   motto = U#user_account_data.motto, about_me = U#user_account_data.about_me, 
				   country = U#user_account_data.country, city = U#user_account_data.city, 
				   sex = U#user_account_data.sex, birth_day = U#user_account_data.birth_day,
				   userpic_url = ?repository:get_url(?USERPIC_PREFIX, U#user_account_data.id),
				   show_tv = U#user_account_data.allow_publishing
				  }
		end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).

get_my_account(UserId, Language) ->
    F = fun() -> 
		case mnesia:read(user_account_data, UserId, write) of
		    [] ->
			undefined;
		    [User] ->
			% temporary patch to fill Language field for all users
			if
			    User#user_account_data.language =/= Language ->
				mnesia:write(User#user_account_data{language = Language});
			    true ->
				ok
			end,
			ModerateContacts = 
			    case mnesia:read(channel_moderator, UserId, read) of
				[] ->
				    [];
				[ChannelModerator] ->
				    ChannelModerator#channel_moderator.channels
			    end,
			#user_account{id = User#user_account_data.id,
				      login = User#user_account_data.login, 
				      password = undefined, %% for security reason
				      creation_date = User#user_account_data.creation_date,
				      motto = User#user_account_data.motto,
				      about_me = User#user_account_data.about_me,
				      name = User#user_account_data.name,
				      country = User#user_account_data.country,
				      city = User#user_account_data.city,
				      email = User#user_account_data.email,
				      sex = User#user_account_data.sex,
				      birth_day = User#user_account_data.birth_day,
				      language = User#user_account_data.language,
				      allow_news = User#user_account_data.allow_news,
				      allow_publishing = User#user_account_data.allow_publishing,
				      allow_show_friends = User#user_account_data.allow_show_friends,
				      userpic_url = ?repository:get_url(?USERPIC_PREFIX, User#user_account_data.id),
				      role = User#user_account_data.role,
				      moderate_contacts = ModerateContacts
				     }
		end
    end,
    mnesia:activity(transaction, F, [], mnesia_frag).

get_contacts(UserId) ->
    get_contacts(UserId, undefined).

get_contacts(UserId, ContactUserId) ->
    F = fun() ->
		case mnesia:dirty_read(contact, UserId) of
		    [] ->
			[];
		    [Contact] ->
			[case mnesia:dirty_read(user_account_data, C#contact_item.contact_id) of
			     [] ->
				 undefined;
			     [U] ->
				 case mnesia:dirty_read(session, C#contact_item.contact_id) of
				     [] ->
					 Status = ?offline;
				     [Session] ->
					 Status = Session#session.status
				 end,
				 #contact_info{user_id = U#user_account_data.id, login = U#user_account_data.login,
					       status = Status,
					       motto = U#user_account_data.motto, name = U#user_account_data.name,
					       birth_day = U#user_account_data.birth_day,
					       authorization_state = C#contact_item.authorization_state,
					       message = C#contact_item.message, is_blocked = C#contact_item.is_blocked
					      }
			 end
			 || C <- Contact#contact.items, 
			    (ContactUserId == undefined orelse C#contact_item.contact_id == ContactUserId)] 
		         ++
			 if 
			     ContactUserId == undefined ->
				 [case mnesia:dirty_read(channel, ChannelItem#contact_channel.channel_id) of
				      [] ->
					  undefined;
				      [Channel] ->
					  #contact_info{user_id = Channel#channel.id, login = Channel#channel.title,
							status = ?online,
							motto = Channel#channel.short_description,
							birth_day = Channel#channel.creation_date, 
							authorization_state = ?authorized, 
							is_blocked = false, type = ?channel}
				  end
				  || ChannelItem <- Contact#contact.channels, ChannelItem#contact_channel.is_deleted == false];
			     true ->
				 [] 
			 end
		end
    end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

get_status(_UserId, ContactUserId) ->
    F = fun() ->
		case mnesia:read(session, ContactUserId, read) of
		    [] ->
			?offline;
		    [Session] ->
			Session#session.status
		end
    end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

get_authorization(UserId, ContactUserId) ->
    F = fun() ->
		case mnesia:read(contact, UserId, read) of
		    [] ->
			[];
		    [Contact] ->
			    case lists:keytake(ContactUserId, #contact_item.contact_id, Contact#contact.items) of
				{value, C, _Rest} ->
				    #authorization{state = C#contact_item.authorization_state, message = C#contact_item.message};
				false ->
				    #authorization{}
			    end
		end
    end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

get_contact_logins(ContactType, ContactId) ->
    F = fun() -> 
		case ContactType of
		    ?friend ->
			case mnesia:read(user_account_data, ContactId, read) of
			    [] ->
				#contact_logins_result{result_code = ?empty};
			    [User] ->
				if 
				    User#user_account_data.allow_show_friends =/= true ->
					#contact_logins_result{result_code = ?hidden};
				    true ->
					case mnesia:read(contact, ContactId, read) of
					    [] ->
						#contact_logins_result{result_code = ?empty};
					    [Contact] ->
						#contact_logins_result{result_code = ok, users =
								       [case mnesia:dirty_read(user_account_data, C#contact_item.contact_id) of
 									    [] ->
 										#contact_login{};
 									    [U] ->
 										#contact_login{contact_id =U#user_account_data.id,
 											       login = U#user_account_data.login}
 									end
 									|| C <- Contact#contact.items, 
 									C#contact_item.authorization_state == ?authorized],
								       channels = 
								       [case mnesia:dirty_read(channel, C#contact_channel.channel_id) of
									    [] ->
										#contact_login{};
									    [Channel] ->
										#contact_login{contact_id =Channel#channel.id,
											       login = Channel#channel.title}
									end
									|| C <- Contact#contact.channels, 
									C#contact_channel.is_deleted == false]
								      }
					end
				end
			end;
		    ?channel ->
			case mnesia:read(channel, ContactId, read) of
			    [] ->
				#contact_logins_result{result_code = ?empty};
			    [Channel] ->
				if
				    Channel#channel.user_count == 0 -> 
					#contact_logins_result{result_code = ?empty};
				    true ->
					#contact_logins_result{result_code = ok, users =
							       [case mnesia:dirty_read(user_account_data, UserId) of
 									    [] ->
 										#contact_login{};
 									    [U] ->
 										#contact_login{contact_id = UserId,
 											       login = U#user_account_data.login}
 									end
 									|| UserId <- Channel#channel.users]}
				end
			end
		end
    end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

remove_from_contacts(UserId, ContactUserID) ->
    F = fun()->
		case mnesia:read(contact, UserId, write) of
		    [] ->
			?contact_not_found;
		    [Contact] ->
			case lists:keytake(ContactUserID, #contact_item.contact_id, Contact#contact.items) of
			    {value, _Item, RestItems} ->
				mnesia:write(Contact#contact{items = RestItems}),
				case mnesia:read(contact, ContactUserID, write) of
				    [] ->
					ok;
				    [Contact2] ->
					case lists:keytake(UserId, #contact_item.contact_id, Contact2#contact.items) of
					    {value, Item2, RestItems2} ->
						mnesia:write(Contact2#contact{items = 
								     [Item2#contact_item{authorization_state = ?not_authorized_me} | RestItems2]});
  					    _Other2 ->
						ok
					end
				end;
			    _Other ->
				ok
			end
		end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).

simple_search_contacts(PageNumber, RecordsPerPage, Value) ->
   if 
       is_list(Value) ->
	   NewValue = ?string:to_upper(Value);
       true ->
	   NewValue = Value
   end,
   F = fun() -> 
	       QH = qlc:q([#user_info{user_id = U#user_account_data.id, login = U#user_account_data.login,  
				      name = U#user_account_data.name, creation_date = U#user_account_data.creation_date, 
				      motto = U#user_account_data.motto, country = U#user_account_data.country,
				      city = U#user_account_data.city, sex = U#user_account_data.sex,
				      birth_day = U#user_account_data.birth_day,
				      userpic_url = ?repository:get_url(?USERPIC_PREFIX, U#user_account_data.id)} || 
			   U <- mnesia:table(user_account_data),
			   ?string:is_empty(Value) orelse
			   ((is_list(U#user_account_data.login) andalso string:str(U#user_account_data.upper_login, NewValue) > 0) orelse
			   (is_list(U#user_account_data.name) andalso string:str(U#user_account_data.upper_name, NewValue) > 0) orelse
			   (is_list(U#user_account_data.email) andalso U#user_account_data.upper_email == NewValue)) ]),
	       QC = qlc:cursor(QH),
	       Result = skip_records(QC, user_search_result, PageNumber, RecordsPerPage, 1, []),
	       qlc:delete_cursor(QC),
	       Result#user_search_result{items = [case mnesia:dirty_read(session, I#user_info.user_id) of
						      [] ->
							  I#user_info{status = ?offline};
						      [Session] ->
							  I#user_info{status = Session#session.status}
						  end || I <- Result#user_search_result.items]}
    end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

advanced_search_contacts(PageNumber, RecordsPerPage, Value, Country, City, Sex, AgeMin, AgeMax) ->
   DateTimeMin = ?datetime:from_age(AgeMax, true), % vice versa
   DateTimeMax = ?datetime:from_age(AgeMin, false),
   if 
       is_list(Value) ->
	   NewValue = ?string:to_upper(Value);
       true ->
	   NewValue = Value
   end,
   F = fun() -> 
	   QH = qlc:q([ #user_info{user_id = U#user_account_data.id, login = U#user_account_data.login,
				   name = U#user_account_data.name, creation_date = U#user_account_data.creation_date, 
				   motto = U#user_account_data.motto, country = U#user_account_data.country,
				   city = U#user_account_data.city, sex = U#user_account_data.sex,
			      	   birth_day = U#user_account_data.birth_day,
				   userpic_url = ?repository:get_url(?USERPIC_PREFIX, U#user_account_data.id)} || 
	         U <- mnesia:table(user_account_data),
		 (?string:is_empty(Value) orelse ((is_list(U#user_account_data.login) andalso 
							  string:str(U#user_account_data.upper_login, NewValue) > 0) orelse
		 (is_list(U#user_account_data.name) andalso string:str(U#user_account_data.upper_name, NewValue) > 0) orelse 
                 (is_list(U#user_account_data.email) andalso U#user_account_data.upper_email == NewValue))) andalso
		 (Country == undefined orelse U#user_account_data.country == Country) andalso
		 (City == [] orelse (is_list(U#user_account_data.city) andalso string:str(U#user_account_data.city, City) > 0)) andalso
		 (Sex == undefined orelse U#user_account_data.sex == Sex) andalso
		 (AgeMax == 0 orelse (U#user_account_data.birth_day >= DateTimeMin andalso U#user_account_data.birth_day < DateTimeMax))
                 ]),
	       QC = qlc:cursor(QH),
	       Result = skip_records(QC, user_search_result, PageNumber, RecordsPerPage, 1, []),
	       qlc:delete_cursor(QC),

	       Result#user_search_result{items = [case mnesia:dirty_read(session, I#user_info.user_id) of
						      [] ->
							  I#user_info{status = ?offline};
						      [Session] ->
							  I#user_info{status = Session#session.status}
						  end || I <- Result#user_search_result.items]}
    end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

send_friend_message(AuthorId, IsPublic, SendDate) ->
    MessageId = ?sequence:sequence(?ART_MESSAGE_COUNTER),
    F = fun() ->
		if
		    IsPublic ->
			case mnesia:read(published_message, AuthorId, write) of
			    [] ->
				mnesia:write(#published_message{author_id = AuthorId, message_image_ids = [{MessageId, SendDate}]});
			    [PublishedQueue] ->
				if 
				    length(PublishedQueue#published_message.message_image_ids) > ?PUBLISHED_MESSAGE_QUEUE_LENGTH - 1 ->
					MessageList = lists:sublist([{MessageId, SendDate} | PublishedQueue#published_message.message_image_ids],
								    ?PUBLISHED_MESSAGE_QUEUE_LENGTH);
				    true ->
					MessageList = [{MessageId, SendDate} | PublishedQueue#published_message.message_image_ids]
				end,
				mnesia:write(PublishedQueue#published_message{message_image_ids = MessageList})
			end;
 		    true ->
			ok
 	        end,
		Recipient_ids = get_user_contact_ids(AuthorId),
		mnesia:write(#art_message{message_id = MessageId, send_date = SendDate, recepient_ids = Recipient_ids, delivery_state = ?saving,
					  author_id = AuthorId, is_public = IsPublic})
     end,
     mnesia:activity(transaction, F, [], mnesia_frag),
     MessageId.

send_private_message(UserId, RecipientId, SendDate) when is_integer(SendDate) ->
    F = fun() ->
		case mnesia:read(contact, UserId, read) of
		    [] ->
			?contact_not_found;
		    [Contact] ->
			  case lists:keytake(RecipientId, #contact_item.contact_id, Contact#contact.items) of
			      {value, _C, _RestItems} ->
				  Recipient_ids = [RecipientId],
				  MessageId = ?sequence:sequence(?ART_MESSAGE_COUNTER),
				  mnesia:write(#art_message{message_id = MessageId, author_id = UserId, send_date = SendDate, 
							    recepient_ids = Recipient_ids}),
				  {ok, MessageId};
			      _Other ->
				  ?contact_not_found
			  end
		end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).

get_next_artmessage_and_report(RecipientId, MessageId) ->
    F = fun() ->
		case mnesia:read(delivery, RecipientId, write) of
		    []  ->
			undefined;
		    [#delivery{message_ids = []}] ->
			undefined;
		    [#delivery{message_ids = MessageIds, count = {FriendsCount, PrivateCount}} = Queue] ->
			delete_recipient_from_artmessage_table(RecipientId, MessageId),
			case lists:keytake(MessageId, #queue_item.message_id, MessageIds) of
			    {value, #queue_item{type = Type, author_id = AuthorId} = _SelectedItem, RestMessageIds} ->
				{LastGotAuthorId, Counts} = case Type of
								friends ->
								    {undefined, {FriendsCount - 1, PrivateCount}};
								private ->
								    {AuthorId, {FriendsCount, PrivateCount - 1}}
							    end,
				NewQueue = Queue#delivery{count = correct_counts(Counts), message_ids = RestMessageIds,
							    last_got_author_id = LastGotAuthorId},
				mnesia:write(NewQueue),
				NewQueue;
			    false ->
				Queue
			end
		end
	end,
    case mnesia:activity(transaction, F, [], mnesia_frag) of
	undefined ->
	    undefined;
	Queue ->
	    case Queue#delivery.message_ids of
		undefined ->
		    undefined;
		[] ->
		    undefined;
		MessageIds ->
		    NextMessage = 
			case Queue#delivery.count of
			    {_FriendsCount, 0} -> % if we have friends messages only - just send oldest one
				lists:last(MessageIds);
			    _ ->
				LastAuthorId = Queue#delivery.last_got_author_id,
						% trying to take an oldest message from author with id > LastAuthorId
				case take_message(LastAuthorId, lists:reverse(MessageIds), {undefined, undefined, undefined}) of
				    {FriendsMsg, PrivateMsg} -> % couldn't get one
					if  % last time we returned friends message, so prefer oldest private with lowest author id
					    LastAuthorId == undefined -> 
						if PrivateMsg =/= undefined -> PrivateMsg; true -> FriendsMsg end;
						% last time we returned some private message, prefer oldest friends message
					    true -> 
						if FriendsMsg =/= undefined -> FriendsMsg; true -> PrivateMsg end
					end;
				    Msg -> % got one
					Msg
				end
			end,
		    case mnesia:activity(async_dirty, fun() -> mnesia:dirty_read(art_message, NextMessage#queue_item.message_id) end, [], mnesia_frag) of
			[] ->
			    {false, NextMessage};
			[ArtMessage] ->
			    {ArtMessage#art_message.is_public, NextMessage}
		    end
	    end
    end.

take_message(_LastAuthorId, [] = _QueueItems, {FriendsMsg, _MinAuthorId, MinAuthorIdMsg}) ->
    {FriendsMsg, MinAuthorIdMsg};
take_message(LastAuthorId, [#queue_item{author_id = AuthorId, type = private} = Item | Items], {FriendsMsg, MinAuthorId, MinAuthorIdMsg}) ->
    if
	LastAuthorId =/= undefined andalso AuthorId > LastAuthorId ->
	    Item;
	true ->
	    {NewMinAuthorId, NewMinAuthorIdMsg} = if
						      MinAuthorId == undefined orelse AuthorId < MinAuthorId ->
							  {AuthorId, Item};
						      true -> {MinAuthorId, MinAuthorIdMsg}
						  end,
	    take_message(LastAuthorId, Items, {FriendsMsg, NewMinAuthorId, NewMinAuthorIdMsg})
    end;
take_message(LastAuthorId, [Item | Items], {FriendsMsg, MinAuthorId, MinAuthorIdMsg}) ->
    NewFriendsMsg = if FriendsMsg == undefined -> Item; true -> FriendsMsg end,
    take_message(LastAuthorId, Items, {NewFriendsMsg, MinAuthorId, MinAuthorIdMsg}).

correct_counts(undefined) ->
    undefined;
correct_counts({FriendsCount, PrivateCount}) ->
    NewFriendsCount = if FriendsCount < 0 -> 0; true -> FriendsCount end,
    NewPrivateCount = if PrivateCount < 0 -> 0; true -> PrivateCount end,
    {NewFriendsCount, NewPrivateCount}.

skip_artmessages(RecipientId, AuthorId) ->
    F = fun() ->
		case mnesia:read(delivery, RecipientId, write) of
		    [] ->
			ok;
		    [#delivery{message_ids = []}] ->
			ok;
		    [#delivery{message_ids = MessageIds} = Queue] ->
			{NewMessageIds, NewCounts} = skip_messages(RecipientId, AuthorId, MessageIds, [], {0, 0}),
			mnesia:write(Queue#delivery{message_ids = NewMessageIds, count = correct_counts(NewCounts)})
		end
        end,
    mnesia:activity(transaction, F, [], mnesia_frag).
skip_messages(_RecipientId, _AuthorToSkipId, [] = _MessageIds, NewMessageIds, Counts) ->
    {lists:reverse(NewMessageIds), Counts};
skip_messages(RecipientId, AuthorToSkipId, [#queue_item{message_id = MessageId, author_id = AuthorId, type = Type} = Item | Items], NewMessageIds, {FriendsCount, PrivateCount} = Counts) ->
    if
	AuthorToSkipId == AuthorId ->	
	    delete_recipient_from_artmessage_table(RecipientId, MessageId),
	    skip_messages(RecipientId, AuthorToSkipId, Items, NewMessageIds, Counts);
    true ->
	    NewCounts = case Type of
			 friends ->
			     {FriendsCount + 1, PrivateCount};
			 private ->
			     {FriendsCount, PrivateCount + 1}
		     end,
	    skip_messages(RecipientId, AuthorToSkipId, Items, [Item | NewMessageIds], NewCounts)
    end.    

process_authorization_request(AuthorId, RecipientId, AuthorizationMessage, MaxContactsCount) ->
    {_, Result} = process_authorization_request_internal(AuthorId, RecipientId, undefined, AuthorizationMessage, MaxContactsCount),
    Result.

process_authorization_request_by_login(AuthorId, RecipientLogin, AuthorizationMessage, MaxContactsCount) ->
    process_authorization_request_internal(AuthorId, undefined, RecipientLogin, AuthorizationMessage, MaxContactsCount).

process_authorization_request_internal(AuthorId, RecipientIdOrUndefined, RecipientLogin, AuthorizationMessage, _MaxContactsCount) ->
    F = fun() ->
		if
		    RecipientLogin == undefined ->
			case mnesia:read(user_account_data, RecipientIdOrUndefined, read) of
			    [] ->
				RecipientId = undefined;
			    [_] ->
				RecipientId = RecipientIdOrUndefined
			end;
		    true ->
			case mnesia:index_read(user_account_data, RecipientLogin, #user_account_data.login) of
			    [] ->
				RecipientId = undefined;
			    [#user_account_data{id = UserId}] ->
				RecipientId = UserId
			end
		end,
		if
		    RecipientId == undefined ->
			{undefined, ?account_not_found};
		    RecipientId == AuthorId ->
			?error(#cannot_authorize_yourself_exception{user_id = RecipientId});
		    true ->	       
			case mnesia:read(contact, AuthorId, write) of
			    [] ->
				AuthorContact = #contact{},
				AuthorRecipientItem = undefined,
				AuthorRestItems = [];
			    [AuthorContact] ->
				AuthorContact = AuthorContact,
				case lists:keytake(RecipientId, #contact_item.contact_id, AuthorContact#contact.items) of
				    {value, AuthorRecipientItem, AuthorRestItems} ->
					AuthorRecipientItem = AuthorRecipientItem,
					AuthorRestItems = AuthorRestItems;
				    false ->
					AuthorRecipientItem = undefined,
					AuthorRestItems = AuthorContact#contact.items
				end
			end,
			case mnesia:read(contact, RecipientId, write) of
			    [] ->
				RecipientContact = #contact{},
				RecipientAuthorItem = undefined,
				RecipientRestItems = [];
			    [RecipientContact] ->
				RecipientContact = RecipientContact,
				case lists:keytake(AuthorId, #contact_item.contact_id, RecipientContact#contact.items) of
				    {value, RecipientAuthorItem, RecipientRestItems} ->
					RecipientAuthorItem = RecipientAuthorItem,
					RecipientRestItems = RecipientRestItems;
				    false ->
					RecipientAuthorItem = undefined,
					RecipientRestItems = RecipientContact#contact.items
				end
			end,
			if
			    RecipientAuthorItem == undefined andalso AuthorRecipientItem == undefined ->
				mnesia:write(AuthorContact#contact{owner_id = AuthorId, items = [#contact_item{contact_id = RecipientId,
													       authorization_state = ?not_authorized_me} | AuthorRestItems] }),
				mnesia:write(RecipientContact#contact{owner_id = RecipientId, items = [#contact_item{contact_id = AuthorId,
														     message = AuthorizationMessage,
														     authorization_state = ?waits_authorization_from_me} | RecipientRestItems]}),
				Result = ok;
			    true ->
				if 
				    RecipientAuthorItem == undefined ->
					if 
					    AuthorRecipientItem#contact_item.authorization_state == ?not_authorized_me ->
						mnesia:write(RecipientContact#contact{items = [#contact_item{contact_id = AuthorId,
													     message = AuthorizationMessage,
													     authorization_state = ?waits_authorization_from_me} | RecipientRestItems]}),
						Result = ok;
					    true ->
						Result = ?incorrect_db_data
					end;
				    true ->
					if
					    AuthorRecipientItem == undefined ->
						if 
						    RecipientAuthorItem#contact_item.authorization_state == ?not_authorized_me ->
							mnesia:write(AuthorContact#contact{items = [#contact_item{contact_id = RecipientId,
														  authorization_state = ?not_authorized_me} | AuthorRestItems] }),
							mnesia:write(RecipientContact#contact{items = [#contact_item{contact_id = AuthorId,
														     message = AuthorizationMessage,
														     authorization_state = ?waits_authorization_from_me} | RecipientRestItems]}),
							Result = ok;
						    true -> 
							Result = ?incorrect_db_data
						end;
					    true ->
						if 
						    RecipientAuthorItem#contact_item.is_blocked -> % special situation when Recipient have blocked you
							Result = ?user_blocked_you;
						    RecipientAuthorItem#contact_item.authorization_state == ?authorized andalso 
						    AuthorRecipientItem#contact_item.authorization_state == ?authorized ->
							Result = ?already_authorized;
						    RecipientAuthorItem#contact_item.authorization_state == ?waits_authorization_from_me andalso 
						    AuthorRecipientItem#contact_item.authorization_state == ?not_authorized_me ->
							Result = ok;
						    RecipientAuthorItem#contact_item.authorization_state == ?not_authorized_me andalso 
						    AuthorRecipientItem#contact_item.authorization_state == ?waits_authorization_from_me ->
							mnesia:write(AuthorContact#contact{items = [#contact_item{contact_id = RecipientId,
														  authorization_state = ?authorized} | AuthorRestItems] }),
							mnesia:write(RecipientContact#contact{items = [#contact_item{contact_id = AuthorId,
														     message = AuthorizationMessage,
														     authorization_state = ?authorized} | RecipientRestItems]}),
							Result = ?authorized_by_this_request;
						    RecipientAuthorItem#contact_item.authorization_state == ?not_authorized_me andalso 
						    AuthorRecipientItem#contact_item.authorization_state == ?not_authorized_me ->
							mnesia:write(RecipientContact#contact{items = [#contact_item{contact_id = AuthorId,
														     message = AuthorizationMessage,
														     authorization_state = ?waits_authorization_from_me} | RecipientRestItems]}),
							Result = ok;
						    true ->
							Result = ?incorrect_db_data
						end
					end
				end
			end,
			{RecipientId, Result}
		end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).

process_authorization_response(AuthorId, RecipientId, IsAccepted) -> 
    F = fun() ->
	   case mnesia:read(contact, AuthorId, write) of
	       [] ->
		   AuthorContact = undefined,
		   AuthorRecipientItem = undefined,
		   AuthorRestItems = undefined;
	       [AuthorContact] ->
		   AuthorContact = AuthorContact,
		   case lists:keytake(RecipientId, #contact_item.contact_id, AuthorContact#contact.items) of
		       {value, AuthorRecipientItem, AuthorRestItems} ->
			   AuthorRecipientItem = AuthorRecipientItem,
			   AuthorRestItems = AuthorRestItems;
		       false ->
			   AuthorRecipientItem = undefined,
			   AuthorRestItems = AuthorContact#contact.items
		   end
	   end,
	   case mnesia:read(contact, RecipientId, write) of
	       [] ->
		   RecipientContact = undefined,
		   RecipientAuthorItem = undefined,
		   RecipientRestItems = undefined;
	       [RecipientContact] ->
		   RecipientContact = RecipientContact,
		   case lists:keytake(AuthorId, #contact_item.contact_id, RecipientContact#contact.items) of
		       {value, RecipientAuthorItem, RecipientRestItems} ->
			   RecipientAuthorItem = RecipientAuthorItem,
			   RecipientRestItems = RecipientRestItems;
		       false ->
			   RecipientAuthorItem = undefined,
			   RecipientRestItems = RecipientContact#contact.items
		   end
	   end,
	   if 
	       AuthorRecipientItem =/= undefined andalso RecipientAuthorItem =/= undefined ->
		   if 
		       AuthorRecipientItem#contact_item.authorization_state == ?waits_authorization_from_me andalso
		       RecipientAuthorItem#contact_item.authorization_state == ?not_authorized_me ->
			   if IsAccepted ->
				   mnesia:write(AuthorContact#contact{items = [#contact_item{contact_id = RecipientId,
								     authorization_state = ?authorized} | AuthorRestItems] }),
				   mnesia:write(RecipientContact#contact{items = [#contact_item{contact_id = AuthorId,
								authorization_state = ?authorized} | RecipientRestItems]});
			      true ->
				   mnesia:write(AuthorContact#contact{items = AuthorRestItems}),
				   ?authorization_request_declined
                           end;				
		       true ->
			   ?not_waiting_for_authorization
		   end;
	       true ->
		   ?not_waiting_for_authorization
	   end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).

logon(Login, Password, CurrentDate, MaxFailedAttempsCount, FailedAttempsInterval, LockInterval) when is_integer(CurrentDate) ->
    F = fun() ->
        MatchHead = #user_account_data{id='$1', password = '$2', login = Login, is_logon_allowed = true, _='_'},
        Select = ['$1', '$2'],
        case mnesia:select(user_account_data, [{MatchHead, [], [Select]}]) of
	    [] ->
		Result = invalid_credentials;
	    [[UserId, UserPassword]] ->
		MatchHead2 = #logon_info{lock_time='$1', failed_attempts_count = '$2', last_failed_attempt_time = '$3', 
					user_id = UserId, _='_'},
		Select2 = ['$1', '$2', '$3'],
		case mnesia:select(logon_info, [{MatchHead2, [], [Select2]}]) of
		    [] ->
			LockTime = undefined, 
			FailedAttemptsCount = undefined, 
			LastFailedAttemptTime = undefined;
		    [[LockTimeTemp, FailedAttemptsCountTemp, LastFailedAttemptTimeTemp]] ->
			LockTime = LockTimeTemp, 
			FailedAttemptsCount = FailedAttemptsCountTemp, 
			LastFailedAttemptTime = LastFailedAttemptTimeTemp
		end,   
		case LockTime == undefined orelse LockInterval == undefined orelse ?datetime:add(LockTime, LockInterval) < CurrentDate of
		    true ->
			CryptedPassword = crypto:rc4_encrypt(xmerl_ucs:to_utf8(Password), xmerl_ucs:to_utf8(Password)),
			if 
		            UserPassword == CryptedPassword ->
				if
		        	    LockTime =/= undefined orelse 
		        	    LastFailedAttemptTime =/= undefined orelse 
				    FailedAttemptsCount > 0 ->
					mnesia:write(#logon_info{user_id = UserId, lock_time = undefined, 
								failed_attempts_count = FailedAttemptsCount, 
								last_failed_attempt_time = LastFailedAttemptTime});
				    true ->
					ok
				end,
				Result = {ok, UserId};
			    is_integer(MaxFailedAttempsCount) ->
				case is_integer(LastFailedAttemptTime) andalso is_integer(FailedAttempsInterval) andalso
				    ?datetime:add(LastFailedAttemptTime, FailedAttempsInterval) >= CurrentDate of
				    true ->
					if
					    FailedAttemptsCount + 1 >= MaxFailedAttempsCount ->
						%% lock
						Result = {locked_too_many_attempts, LockInterval},
						mnesia:write(#logon_info{user_id = UserId, lock_time = CurrentDate, 
									failed_attempts_count = 0, 
									last_failed_attempt_time = undefined});
					    true ->
						%% register other failed attempt
						Result = invalid_credentials,
						mnesia:write(#logon_info{user_id = UserId, lock_time = undefined, 
									failed_attempts_count = FailedAttemptsCount + 1, 
									last_failed_attempt_time = CurrentDate})
					end;
				    false ->
					if
					    MaxFailedAttempsCount == 1 ->
						%% lock
						Result = {locked_too_many_attempts, LockInterval},
						mnesia:write(#logon_info{user_id = UserId, lock_time = CurrentDate, 
									 failed_attempts_count = 0, 
									 last_failed_attempt_time = undefined});
					    true ->
						%% register failed attempt
						Result = invalid_credentials,
						mnesia:write(#logon_info{user_id = UserId, lock_time = undefined, 
									 failed_attempts_count = 1, 
									 last_failed_attempt_time = CurrentDate})
					end
				end;
			    true ->
				Result = invalid_credentials
			end;
		    false ->
			Result = {locked_too_many_attempts, ?datetime:date_diff(?datetime:add(LockTime, LockInterval), CurrentDate)}
		end
        end,
	Result
    end,
    mnesia:activity(transaction, F, [], mnesia_frag).

block_contact(UserId, ContactUserId) ->
    % when User is blocked some your's contact also Authorization status of both contacts is changed to 'not_authorized_me'
    % As result friendly and private messages and Notification will not delivery to recipient
    F = fun() ->
		case mnesia:read(contact, UserId, write) of
		    [] ->
			?contact_not_found;
		    [Contact] ->
			case lists:keytake(ContactUserId, #contact_item.contact_id, Contact#contact.items) of
			    {value, Item, RestItems} ->
				mnesia:write(Contact#contact{items = [Item#contact_item{is_blocked = true, authorization_state = ?not_authorized_me} |
								      RestItems]}),
				case mnesia:read(contact, ContactUserId, write) of
				    [] ->
					ok;
				    [Contact2] -> 
					case lists:keytake(UserId, #contact_item.contact_id, Contact2#contact.items) of
					    {value, Item2, RestItems2} ->
						mnesia:write(Contact2#contact{items = [Item2#contact_item{authorization_state = ?not_authorized_me} |
										      RestItems2]});
					    false ->
						ok
					end
				end;
			    false ->
				?contact_not_found
			end
		end
        end,
    mnesia:activity(transaction, F, [], mnesia_frag).

unblock_contact(UserId, ContactUserId) ->
    F = fun() ->
		case mnesia:read(contact, UserId, write) of
		    [] ->
			?contact_not_found;
		    [Contact] ->
			case lists:keytake(ContactUserId, #contact_item.contact_id, Contact#contact.items) of
			    {value, Item, RestItems} ->
				mnesia:write(Contact#contact{items = [Item#contact_item{is_blocked = false} | RestItems]});
			    false ->
				?contact_not_found
			end
		end

        end,
    mnesia:activity(transaction, F, [], mnesia_frag).

create_moodstrip(UserId, SendDate, Title, Language, IsHidden, ChannelId) ->
    F = fun() ->
		MoodStripId = ?sequence:sequence(moodstrip),
		case mnesia:read(user_account_data, UserId, read) of
		    [] ->
			-1;
		    [User] ->
			mnesia:write(#moodstrip{id = MoodStripId, author_id = UserId, author_login = User#user_account_data.login, 
						send_date = SendDate, title = Title, upper_title = ?string:to_upper(Title),
						language = Language, is_hidden = IsHidden, channel_id = ChannelId}),
			MoodStripId
		end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).

add_image_to_moodstrip(MoodStripId, MessageId, AuthorId, Author, SendDate, IsLastImage, SortedTable) ->
    ImageId = ?sequence:sequence(?MOODSTRIP_ITEM_COUNTER),
    F = fun() ->
		case mnesia:read(moodstrip, MoodStripId, write) of
		    [] ->
			?moodstrip_not_found;
		    [MoodStrip] ->
			if 
			    MoodStrip#moodstrip.author_id =/= AuthorId ->
			    	?moodstrip_not_found;
			    MoodStrip#moodstrip.is_published == true ->
			    	?moodstrip_is_published;
			    true ->
				case lists:keytake(MessageId, #moodstrip_item.message_id, MoodStrip#moodstrip.items) of
				    {value, MoodstripItem, _RestItems} ->
					{ok, MoodstripItem#moodstrip_item.image_id, MoodStrip};
				    false ->
					NewMoodstrip = MoodStrip#moodstrip{items = 
									 [#moodstrip_item{message_id = MessageId, image_id = ImageId, 
									 		  author_login = Author, send_date = SendDate} | 
									  MoodStrip#moodstrip.items]},
					if 
					    IsLastImage == true ->
						publish_moodstrip_internal(NewMoodstrip, SortedTable);
					    true ->
						mnesia:write(NewMoodstrip)
					end,
					{ok, ImageId, NewMoodstrip}
				end
			end
		end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).

mark_moodstrip_for_deleting(AuthorId, MoodStripId) ->
    F = fun() ->
		case mnesia:read(moodstrip, MoodStripId, write) of
		    [] ->
			?moodstrip_not_found;
		    [MoodStrip] ->
			if 
			    MoodStrip#moodstrip.author_id =/= AuthorId ->
				?moodstrip_not_found;
			    true ->
				mnesia:write(MoodStrip#moodstrip{is_published = false}),
				mnesia:delete({moodstrip_sorted, MoodStrip#moodstrip.send_date}),
				{ok, MoodStrip#moodstrip.items}
			end
		end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).

delete_moodstrip(MoodStripId) ->
    F = fun() ->
		mnesia:delete({moodstrip, MoodStripId})
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).

get_moodstrip(MoodStripId, _IsPreview) ->
    MoodStripItemMap = fun(MoodStripItem) ->
				      #moodstrip_item_result{url = ?repository:get_url(?MOODSTRIP_PREFIX, MoodStripItem#moodstrip_item.image_id),
							     author_login = MoodStripItem#moodstrip_item.author_login,
							     send_date = MoodStripItem#moodstrip_item.send_date}
			  end,
    F = fun() ->
        case mnesia:read(moodstrip, MoodStripId, read) of
	    [] ->
		undefined;
	    [MoodStrip] ->
		if 
		    MoodStrip#moodstrip.is_published == true ->
			#moodstrip_result{author_id = MoodStrip#moodstrip.author_id,
				  author_login = MoodStrip#moodstrip.author_login,
				  title = MoodStrip#moodstrip.title, 
				  send_date = MoodStrip#moodstrip.send_date,
				  items = lists:map(MoodStripItemMap, MoodStrip#moodstrip.items)};
		    true ->
			undefined
		end
	end
   end,
   mnesia:activity(async_dirty, F, [], mnesia_frag).

search_moodstrip(PageNumber, RecordsPerPage, Title, Language) ->
   if 
       is_list(Title) ->
	   NewTitle = ?string:to_upper(Title);
       true ->
	   NewTitle = Title
   end,
   F = fun() -> 
 	       QH = qlc:q([#moodstrip_result{moodstrip_id = M#moodstrip.id, author_id = M#moodstrip.author_id, 
					     author_login = M#moodstrip.author_login,
 					     title = M#moodstrip.title, send_date = M#moodstrip.send_date, 
 					     url = get_moodstrip_url(M#moodstrip.items),
					     count = ?string:len(M#moodstrip.items)
					    } || 
			   M <- mnesia:table(moodstrip),
			   M#moodstrip.is_published == true andalso M#moodstrip.is_hidden == false andalso M#moodstrip.language == Language andalso
			   (?string:is_empty(Title) orelse string:str(M#moodstrip.upper_title, NewTitle) > 0) ]),
 	       QC = qlc:cursor(QH),
 	       Result = skip_records(QC, moodstrip_search_result, PageNumber, RecordsPerPage, 1, []),
 	       qlc:delete_cursor(QC),
 	       Result
   end,
   mnesia:activity(async_dirty, F, [], mnesia_frag).

get_last_moodstrips(Count, DateTimeMin, SortedTable) ->
   F = fun() -> 
	       FirstKey = mnesia:last(SortedTable),
	       if
		   FirstKey == '$end_of_table' orelse FirstKey < DateTimeMin ->
		       undefined;
		   true ->
		       {_HasMore, Data} = get_sorted_moodstrips(DateTimeMin, Count, 0, FirstKey, [], SortedTable),
		       Data
	       end
   end,
   mnesia:activity(async_dirty, F, [], mnesia_frag).

get_sorted_moodstrips(PageNumber, RecordsPerPage, _SortedBy, DateTimeMin, SortedTable) ->
   F = fun() -> 
	       FirstRecordNumber = PageNumber * RecordsPerPage,
	       FirstKey = mnesia:last(SortedTable),
	       if 
		   FirstKey == '$end_of_table' orelse FirstKey < DateTimeMin ->
		       #moodstrip_search_result{page_number = 1, has_more = false, items = []};
		   true ->
		       {RealPageNumber, StartDate} = get_start_key(SortedTable, RecordsPerPage, DateTimeMin, FirstRecordNumber, 0, 
								   FirstKey, FirstKey),
		       {HasMore, Data} = get_sorted_moodstrips(DateTimeMin, RecordsPerPage, 0, StartDate, [], SortedTable),
		       #moodstrip_search_result{page_number = RealPageNumber, has_more = HasMore, items = Data}
	       end
   end,
   mnesia:activity(async_dirty, F, [], mnesia_frag).

get_contact_moodstrips(PageNumber, RecordsPerPage, ContactType, ContactId) ->
    F = fun() -> 
 	       QH = qlc:keysort(#moodstrip_result.moodstrip_id,
				qlc:q([#moodstrip_result{moodstrip_id = M#moodstrip.id, author_id = M#moodstrip.author_id,
							 author_login = M#moodstrip.author_login,
							 title = M#moodstrip.title, send_date = M#moodstrip.send_date,
							 url = get_moodstrip_url(M#moodstrip.items),
							 count = ?string:len(M#moodstrip.items)
							} || 
				       M <- mnesia:table(moodstrip),
				       M#moodstrip.is_published == true andalso M#moodstrip.is_hidden == false andalso
  				       ((ContactType == ?friend andalso M#moodstrip.author_id == ContactId) orelse
					(ContactType == ?channel andalso M#moodstrip.channel_id == ContactId))
				       ]),
				{order, descending}),
 	       QC = qlc:cursor(QH),
 	       Result = skip_records(QC, moodstrip_search_result, PageNumber, RecordsPerPage, 1, []),
 	       qlc:delete_cursor(QC),
 	       Result
   end,
   mnesia:activity(async_dirty, F, [], mnesia_frag).

get_contact_moodstrips_4rss(ContactType, ContactId, Count) ->
    MoodStripItemMap = fun(MoodStripItem) ->
				      #moodstrip_item_result{url = ?repository:get_url(?MOODSTRIP_PREFIX, MoodStripItem#moodstrip_item.image_id),
							     author_login = MoodStripItem#moodstrip_item.author_login,
							     send_date = MoodStripItem#moodstrip_item.send_date}
		       end,
    F = fun() -> 
 	       QH = qlc:keysort(#moodstrip_result.moodstrip_id,
				qlc:q([#moodstrip_result{moodstrip_id = M#moodstrip.id, author_id = M#moodstrip.author_id,
							 author_login = M#moodstrip.author_login,
							 title = M#moodstrip.title, items = lists:map(MoodStripItemMap, M#moodstrip.items)
							} || 
				       M <- mnesia:table(moodstrip),
				       M#moodstrip.is_published == true andalso M#moodstrip.is_hidden == false andalso
  				       ((ContactType == ?friend andalso M#moodstrip.author_id == ContactId) orelse
					(ContactType == ?channel andalso M#moodstrip.channel_id == ContactId))
				       ]),
				{order, descending}),
 	       QC = qlc:cursor(QH),
 	       Result = qlc:next_answers(QC, Count),
 	       qlc:delete_cursor(QC),
 	       Result
   end,
   mnesia:activity(async_dirty, F, [], mnesia_frag).

get_sorted_users(PageNumber, RecordsPerPage, _SortedBy, DateTimeMin) ->
   F = fun() -> 
	       FirstRecordNumber = PageNumber * RecordsPerPage,
	       FirstKey = mnesia:last(user_account_sorted),
	       if 
		   FirstKey == '$end_of_table' orelse FirstKey < DateTimeMin ->
		       #user_search_result{page_number = 1, has_more = false, items = []};
		   true ->
		       {RealPageNumber, StartDate} = get_start_key(user_account_sorted, RecordsPerPage, DateTimeMin, FirstRecordNumber, 0, 
								   FirstKey, FirstKey),
		       {HasMore, Data} = get_sorted_user_accounts(DateTimeMin, RecordsPerPage, 0, StartDate, []),
		       #user_search_result{page_number = RealPageNumber, has_more = HasMore, items = Data}
	       end
   end,
   mnesia:activity(async_dirty, F, [], mnesia_frag).

reset_password(Login, ResetCode, NewPassword) ->
    F = fun() ->
		case mnesia:index_read(user_account_data, Login, #user_account_data.login) of
		    [] ->
			?invalid_login;
		    [User] ->
			if User#user_account_data.email == [] orelse User#user_account_data.email == undefined ->
				?invalid_email;
			   true ->
				{mnesia:write(#user_password_reset_request{user_id = User#user_account_data.id, 
									   request_date = ?datetime:now(), 
									   new_password = NewPassword,
									   reset_code = ResetCode}), 
				 User#user_account_data.id, User#user_account_data.name, User#user_account_data.email}
			end
		end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).

get_recovered_password(UserId, ResetCode) ->
    F = fun() ->
		case mnesia:read(user_password_reset_request, UserId, read) of
		    [] ->
			[];
		    [Request] ->
			if 
			    ResetCode == Request#user_password_reset_request.reset_code ->
				case mnesia:read(user_account_data, UserId, write) of
				    [] ->
					[];
				    [User] ->
					mnesia:write(User#user_account_data{password = 
							  crypto:rc4_encrypt(xmerl_ucs:to_utf8(Request#user_password_reset_request.new_password), 
									     xmerl_ucs:to_utf8(Request#user_password_reset_request.new_password))})
				end,
				mnesia:delete({user_password_reset_request, UserId}),
				Request#user_password_reset_request.new_password;
			    true ->
				[]
			end
		end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).

unsubscribe(UserId, UnsubscribeCode) ->
    F = fun() ->
		case mnesia:read(user_account_data, UserId, write) of
		    [] ->
			?unsubscribe_error;
		    [User] ->
			OurUnsubscribeCode = base64:encode_to_string(crypto:aes_cfb_128_encrypt(?AES_KEY, ?AES_VECTOR, 
										term_to_binary([UserId, User#user_account_data.email, ?SECRET_CODE]))),
			if 
			    UnsubscribeCode == OurUnsubscribeCode ->
				mnesia:write(User#user_account_data{allow_news = false});
			    true ->
				?unsubscribe_error
			end
		end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).

get_published_images(ContactType, ContactId) ->
    F = fun() ->
		case ContactType of
		    ?friend ->
			case mnesia:read(user_account_data, ContactId, read) of
			    [] ->
				#published_images_result{result_code = ?empty};
			    [User] ->
				if
				    User#user_account_data.allow_publishing -> 
					case mnesia:read(published_message, ContactId, read) of
					    [] ->
						#published_images_result{result_code = ?empty};
					    [PublishedMessage] ->
						#published_images_result{result_code = ok, images =
									 lists:map(fun(MessageItem) -> 
											   {ImageId, SendDate} = MessageItem,
											   #published_image{send_date = SendDate,
												  url = ?repository:get_url(?ARTMESSAGE_PREFIX, ImageId),
												  author_login = User#user_account_data.login}
										   end, 
										   PublishedMessage#published_message.message_image_ids)
									 }
					end;
				    true ->
					#published_images_result{result_code = ?hidden}
				end
			end;
		    ?channel ->
			case mnesia:read(channel_message, ContactId, read) of
			    [] ->
				#published_images_result{result_code = ?empty};
			    [ChannelMessage] ->
				#published_images_result{result_code = ok, images =
					 lists:map(fun(MessageItem) -> 
							   #published_image{send_date = MessageItem#channel_message_item.send_date,
							       url = ?repository:get_url(?ARTMESSAGE_PREFIX, MessageItem#channel_message_item.message_id),
							       author_login = MessageItem#channel_message_item.author_login}
						   end, 
						   ChannelMessage#channel_message.messages)}
			end
		end
	end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

get_subscribers() ->
    F = fun() -> 
	       qlc:e(qlc:q([{U#user_account_data.id, U#user_account_data.email} ||
			   U <- mnesia:table(user_account_data),
			   U#user_account_data.allow_news == true ]))
    end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

send_email(SentUserId, MoodstripID) ->
    F = fun() ->
		case mnesia:read(user_account_data, SentUserId, read) of
		    [] ->
			?empty;
		    [SentUser] ->
			case mnesia:read(moodstrip, MoodstripID, read) of
			    [] ->
				?empty;
			    [Moodstrip] ->
				case mnesia:read(user_account_data, Moodstrip#moodstrip.author_id, read) of
				    [] ->
					?empty;
				    [ReceivedUser] ->
					{ok, ReceivedUser#user_account_data.email, SentUser#user_account_data.login, Moodstrip#moodstrip.title}
				end
			end
		end
	end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command functions

get_commands(UserId, PreviousPackageId) ->
    F = fun() ->
		case mnesia:read(delivery, UserId, write) of
		    [] ->
			undefined;
		    [Delivery] ->
			if 
			    PreviousPackageId =/= undefined andalso PreviousPackageId =/= 0 ->
				NewCommands = lists:keydelete(PreviousPackageId, #command_package.package_id, Delivery#delivery.commands),
				MustBeSaved = true;
			    true ->
				NewCommands = Delivery#delivery.commands,
				MustBeSaved = false
			end,
			if 
			    NewCommands == [] ->
				if 
				    MustBeSaved ->
					mnesia:write(Delivery#delivery{commands = NewCommands});
				    true ->
					ok
				end,
				undefined;
			    true ->
				Package = lists:last(NewCommands),
				if 
				    Package#command_package.package_id == undefined ->
					PackageId = ?sequence:sequence(command_package_counter),
					NewNewCommands = lists:keyreplace(undefined, #command_package.package_id,
									  NewCommands, Package#command_package{package_id = PackageId}),
					mnesia:write(Delivery#delivery{commands = NewNewCommands}),
					Package#command_package{package_id = PackageId,
								items = [{package_union, C} || C <- Package#command_package.items]};
				    true ->
					if 
					    MustBeSaved ->
						mnesia:write(Delivery#delivery{commands = NewCommands});
					    true ->
						ok
					end,
					Package#command_package{items = [{package_union, C} || C <- Package#command_package.items]}
				end
			end
		end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% notification functions:

notification_register(UserId, Key) ->
    NotificationServer = get_notification_server(),
    F = fun() ->
		PreviousSession = mnesia:read(session, UserId, write),
		mnesia:write(#session{user_id = UserId, key = Key, 
				      server_pid = NotificationServer#notification_server.pid,
				      server_ip = NotificationServer#notification_server.ip,
				      status = ?online}),
		{PreviousSession, NotificationServer}
    end,
    mnesia:activity(transaction, F, [], mnesia_frag).    

notification_unregister(UserId, Key) ->
    F = fun() ->
		case mnesia:read(session, UserId, write) of
		    [] ->
			?session_is_not_exist;
		    [Session] ->
			if 
			    Session#session.key == Key ->
				mnesia:delete({session, UserId}),
				{ok, Session};
			    true ->
				{?invalid_key, Session}
			end
		end
    end,
    mnesia:activity(transaction, F, [], mnesia_frag).

get_server_load(0, _Keys) ->
    error;
get_server_load(Count, Keys) ->
    case mnesia:read(notification_server, lists:nth(random:uniform(length(Keys)), Keys), read) of
	[] ->
	    error;
	[Server] ->
	    try
	      {Server, moodbox_notification:get_loading(Server#notification_server.pid)}
	    catch
		_Class:_Exception ->
		    case check_notification_server(Server) of
			?notification_server_was_deleted ->
			    get_server_load(Count - 1, lists:delete(Server#notification_server.pid, Keys));
			ok ->
			    get_server_load(Count - 1, Keys)
		    end
	    end
    end.

get_notification_server() ->
    F = fun() ->
		case mnesia:all_keys(notification_server) of
		    [] ->
			?error(#cannot_select_notification_server{});
		    Keys ->
			{A,B,C} = now(),
			random:seed(A,B,C),
			case get_server_load(?NOTIFICATION_COUNT_OF_CONNECTION_ATTEMPS, Keys) of
			    {Server1, Loading1} ->
				case get_server_load(?NOTIFICATION_COUNT_OF_CONNECTION_ATTEMPS, Keys) of
				    {Server2, Loading2} ->
					if 
					    Loading1 >= Loading2 ->
						Server2;
					    true ->
						Server1
					end;
				    error ->
					?error(#cannot_select_notification_server{})
				end;
			    error ->
				?error(#cannot_select_notification_server{})
			end
                end		
    end,
    mnesia:activity(async_dirty, F, []).

add_notification_server(Pid, Address) ->
    F = fun() ->
		mnesia:write(#notification_server{pid = Pid, ip = Address, modified_date = ?datetime:now()})
    end,
    mnesia:activity(transaction, F, []).

check_notification_server(NotificationServer) ->
    case ?datetime:now() - NotificationServer#notification_server.modified_date > ?NOTIFICATION_SERVER_KILL_INTERVAL of
	true ->
	    spawn(?MODULE, delete_notification_server, [NotificationServer#notification_server.pid]),
	    ?notification_server_was_deleted;
	false ->
	    ok
    end.

delete_notification_server(ServerPid) ->
    F = fun() ->
		mnesia:delete({notification_server, ServerPid}),
		UserIds = qlc:e(qlc:q([S#session.user_id || 
				       S <- mnesia:table(session),
				       S#session.server_pid == ServerPid])),
 	       delete_user_sessions(UserIds)
    end,
    mnesia:activity(transaction, F, []).

check_notification_key(UserId, Key) ->
    F = fun() ->
		case mnesia:read(session, UserId, read) of
		    [] ->
			undefined;
		    [Session] ->
			if 
			    Session#session.key == Key ->
				{ok, Session#session.server_pid};
			    true ->
				?invalid_notification_key
			end			
                end		
    end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

delete_user_sessions([]) ->
    ok;
delete_user_sessions([Id | Rest]) ->
    mnesia:delete({session, Id}),
    mnesia:delete({notification_error, Id}),
    delete_user_sessions(Rest).

notify(FromUserId, ForUserId, Event) ->
    F = fun() ->
		case mnesia:read(session, ForUserId, read) of
		    [] ->
			ok;
		    [S] ->
			try
			    moodbox_notification:add_notification(S#session.server_pid, [ForUserId], FromUserId, Event)
			catch
			    _Class:_Exception ->
				add_notification_error(S#session.server_pid, ForUserId)
			end
		end
    end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

notify_by_user(FromUserId, Event) ->
    F = fun() ->
	   case mnesia:read(contact, FromUserId, read) of
	       [] ->
		   [];
	       [Contact] ->
		   Dict = dict:new(),
		   dict:fold(fun(ServerPid, Users, _) -> 
				     moodbox_notification:add_notification(ServerPid, Users, FromUserId, Event)
			     end, 
			     ok, notify_internal(Contact#contact.items, Dict))
	   end
	end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

notify_by_channel(ChannelId, Event, Command) ->
    F = fun() ->
	   case mnesia:dirty_read(channel, ChannelId) of
	       [] ->
		   [];
	       [Channel] ->
	           Dict = dict:new(),
		   dict:fold(fun(ServerPid, Users, _) -> 
				     moodbox_notification:add_channel_notification(ServerPid, Users, ChannelId, Event)
			     end, 
			     ok, notify_by_channel_internal(Channel#channel.users, Dict, Command))
	   end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).

notify_internal([], Dict) ->
    Dict;
notify_internal([C | RestContacts], Dict) ->
    if
	C#contact_item.authorization_state == ?authorized ->
	    case mnesia:dirty_read(session, C#contact_item.contact_id) of
		[] ->
		    notify_internal(RestContacts, Dict);
		[S] ->
		    notify_internal(RestContacts, dict:append(S#session.server_pid, C#contact_item.contact_id, Dict))
	    end;
	true ->
	    notify_internal(RestContacts, Dict)
    end.

notify_by_channel_internal([], Dict, _Command) ->
    Dict;
notify_by_channel_internal([UserId | RestUsers], Dict, Command) ->
    if 
	Command =/= undefined ->
	    case mnesia:read(delivery, UserId, write) of
		[] ->
		    mnesia:write(#delivery{recipient_id = UserId, commands = [#command_package{items = [Command]}]});
		[Delivery] ->
		    case lists:keytake(undefined, #command_package.package_id, Delivery#delivery.commands) of
			{value, Package, RestPackages} ->
			    if 
				length(Package#command_package.items) > ?MAX_COMMANDS_IN_PACKAGE - 1 ->
				    {CommandList, _RestCommandList} = lists:split(?MAX_COMMANDS_IN_PACKAGE, [Command | Package#command_package.items]);
				true ->
				    CommandList = [Command | Package#command_package.items]
			    end,
			    mnesia:write(Delivery#delivery{commands = [Package#command_package{items = CommandList} | RestPackages]});
			false ->
			    mnesia:write(Delivery#delivery{commands = [#command_package{items = [Command]} | Delivery#delivery.commands]})
		    end
	    end;
	true ->
	    ok
    end,
    case mnesia:dirty_read(session, UserId) of
	[] ->
	    notify_by_channel_internal(RestUsers, Dict, Command);
	[S] ->
	    notify_by_channel_internal(RestUsers, dict:append(S#session.server_pid, UserId, Dict), Command)
    end.

add_notification_error(ServerPid, ForUserId) ->
    F = fun() ->
		case mnesia:read(notification_server, ServerPid, read) of
		    [] ->
			ok;
		    [NotificationServer] ->
			case check_notification_server(NotificationServer) of
			    ?notification_server_was_deleted ->
				ok;
			    ok ->
				mnesia:write(#notification_error{user_id = ForUserId, server_pid = ServerPid, creation_date = ?datetime:now()})
			end
		end
    end,
    mnesia:activity(transaction, F, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% channel functions

search_channel(PageNumber, RecordsPerPage, Language, Value) ->
   if 
       is_list(Value) ->
	   NewValue = ?string:to_upper(Value);
       true ->
	   NewValue = Value
   end,
   F = fun() -> 
	       QH = qlc:keysort(#channel_result.user_count, % 2 -  user_count here actually is 'order' field
				qlc:q([#channel_result{channel_id = C#channel.id, creation_date = C#channel.creation_date,
						       title = C#channel.title, short_description = C#channel.short_description,
						       user_count  = if Language == 'ru' -> C#channel.order_ru; true -> C#channel.order end,
						       logo_url = ?repository:get_url(?CHANNEL_LOGO_PREFIX, C#channel.id)} || 
					  C <- mnesia:table(channel),
					  (?string:is_empty(Value) orelse (string:str(C#channel.upper_title, NewValue) > 0)) andalso
					  % @todo add a new field language in the future instead of this hack
					  (Language =/= 'ru' andalso C#channel.order_ru > 999) orelse (Language == 'ru' andalso C#channel.order > 999)
					     ]),
				{order, ascending}),
	       QC = qlc:cursor(QH),
	       Result = skip_records(QC, channel_search_result, PageNumber, RecordsPerPage, 1, []),
	       qlc:delete_cursor(QC),
	       Result
    end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

get_channel_info(ChannelId) ->
    F = fun() ->
        case mnesia:read(channel, ChannelId, read) of
	    [] ->
		undefined;
	    [Channel] ->
		case mnesia:read(user_account_data, Channel#channel.author_id, read) of
		    [] ->
			undefined;
		    [User] ->
			#channel_result{channel_id = Channel#channel.id, creation_date = Channel#channel.creation_date,
					author_id = User#user_account_data.id,
					author_login = User#user_account_data.login,
					title = Channel#channel.title, short_description = Channel#channel.short_description,
					full_description = Channel#channel.full_description,
					user_count  = Channel#channel.user_count,
					logo_url = ?repository:get_url(?CHANNEL_LOGO_PREFIX, Channel#channel.id),
					moderators = [#contact_login{contact_id = ModeratorId, login = ModeratorLogin} ||
						      {ModeratorId, ModeratorLogin} <- Channel#channel.moderators]}
		end
	end
   end,
   mnesia:activity(async_dirty, F, [], mnesia_frag).

add_user_to_channel(ChannelId, UserId) ->
    F = fun() ->
        case mnesia:read(channel, ChannelId, write) of
	    [] ->
		undefined;
	    [Channel] ->
		case lists:member(UserId, Channel#channel.users) of
		    true ->
			ok;
		    false ->
			case mnesia:read(contact, UserId, write) of
			    [] ->
				UserContact = #contact{owner_id = UserId, channels = [#contact_channel{channel_id = ChannelId}]};
			    [Contact] ->
				case lists:keytake(ChannelId, #contact_channel.channel_id, Contact#contact.channels) of
				    {value, ChannelItem, RestChannels} ->
					if 
					    ChannelItem#contact_channel.is_blocked == true ->
						UserContact = Contact#contact{channels = [ChannelItem#contact_channel{is_deleted = false} | RestChannels]};
					    true ->
						UserContact = Contact
					end;					
				    false ->
					UserContact = Contact#contact{channels = [#contact_channel{channel_id = ChannelId} | Contact#contact.channels]}
				end
			end,
			mnesia:write(UserContact),
			mnesia:write(Channel#channel{users = [UserId | Channel#channel.users],
						     user_count = Channel#channel.user_count + 1})
		end
	end
   end,
   mnesia:activity(transaction, F, [], mnesia_frag).

delete_user_from_channel(ChannelId, UserId) ->
    F = fun() ->
        case mnesia:read(channel, ChannelId, write) of
	    [] ->
		undefined;
	    [Channel] ->
		case lists:member(UserId, Channel#channel.users) of
		    true ->
			mnesia:write(Channel#channel{users = lists:delete(UserId, Channel#channel.users),
						     user_count = Channel#channel.user_count - 1});
		    false ->
			ok
		end,
		case mnesia:read(contact, UserId, write) of
		    [] ->
			ok;
		    [Contact] ->
			case lists:keytake(ChannelId, #contact_channel.channel_id, Contact#contact.channels) of
			    {value, ChannelItem, RestChannels} ->
				if 
				    ChannelItem#contact_channel.is_blocked == true ->
					mnesia:write(Contact#contact{channels = [ChannelItem#contact_channel{is_deleted = true} | RestChannels]});
				    true ->
					mnesia:write(Contact#contact{channels = RestChannels})
				end;
			    false ->
				ok
			end
		end
	end
   end,
   mnesia:activity(transaction, F, [], mnesia_frag).

get_next_channel_message(ChannelId, LastMessageId, SkipLastMessage) ->
    F = fun() ->
        case mnesia:read(channel_message, ChannelId, read) of
	    [] ->
		undefined;
	    [ChannelMessage] ->
		if
		    ChannelMessage#channel_message.messages == [] ->
			undefined;
		    LastMessageId == undefined orelse LastMessageId == 0 ->
			if
			    ChannelMessage#channel_message.message_count >= ?CHANNEL_MESSAGE_QUEUE_LENGTH_FOR_NEWBIE ->
				lists:nth(?CHANNEL_MESSAGE_QUEUE_LENGTH_FOR_NEWBIE, ChannelMessage#channel_message.messages);
			    true ->
				lists:last(ChannelMessage#channel_message.messages)
			end;
		    true ->
			case get_next_channel_message_internal(LastMessageId, '$last_message$', '$last_message$',
							       ChannelMessage#channel_message.messages, SkipLastMessage) of
			    '$last_message$' ->
				undefined;
			    undefined ->
				lists:last(ChannelMessage#channel_message.messages);
			    MessageItem ->
				MessageItem
			end				
		end
	end
   end,
   mnesia:activity(async_dirty, F, [], mnesia_frag).

send_channel_message(ChannelId, AuthorId, SendDate) ->
    if 
	ChannelId == 11647 orelse ChannelId == 2882 orelse ChannelId == 2925 orelse ChannelId == 2252 orelse ChannelId == 12590 ->
	    {error, ?closed_contact};
	true ->
	    MessageId = ?sequence:sequence(?ART_MESSAGE_COUNTER),
	    F = fun() ->
			case mnesia:dirty_read(contact, AuthorId) of
			    [] ->
				{error, ?contact_not_found};
			    [Contact] ->
				case lists:member(#contact_channel{channel_id = ChannelId}, Contact#contact.channels) of
				    true ->
					mnesia:write(#art_message{message_id = MessageId, send_date = SendDate, delivery_state = ?saving,
								  author_id = AuthorId, is_public = true}),
					{ok, MessageId};
				    false ->
					{error, ?not_authorized_me}
				end
			end
		end,
	    mnesia:activity(transaction, F, [], mnesia_frag)
    end.
	    

update_channel(AuthorId, ChannelId, Title, ShortDescription, FullDescription, Language, Order) ->
    F = fun() ->
		case mnesia:read(user_account_data, AuthorId, read) of
		    [] ->
			?forbidden;
		    [Author] ->
			if
			    Author#user_account_data.role == ?admin ->
				if
				    ChannelId == 0 orelse ChannelId == undefined ->
					CreationDate = ?datetime:now(),
					NewChannelId = ?sequence:sequence(user),
					if 
					    Language == 'ru' ->
						mnesia:write(#channel{id = NewChannelId, author_id = AuthorId, creation_date = CreationDate, 
								      title = Title, upper_title = ?string:to_upper(Title), 
								      short_description = ShortDescription, full_description = FullDescription, 
								      order_ru = Order, order = 1000});
					    true ->
						mnesia:write(#channel{id = NewChannelId, author_id = AuthorId, creation_date = CreationDate, 
								      title = Title, upper_title = ?string:to_upper(Title), 
								      short_description = ShortDescription, full_description = FullDescription, 
								      order = Order, order_ru = 1000})
					end;
				    true ->
					case mnesia:read(channel, ChannelId, write) of
					    [] ->
						ok;
					    [Channel] ->
						if 
						    Language == 'ru' ->
							mnesia:write(Channel#channel{title = Title,upper_title = ?string:to_upper(Title),
										     short_description = ShortDescription, 
										     full_description = FullDescription, order_ru = Order});
						    true ->
							mnesia:write(Channel#channel{title = Title,upper_title = ?string:to_upper(Title),
										     short_description = ShortDescription, 
										     full_description = FullDescription, order = Order})
						end
					end
				end;
			    true ->
				?forbidden
			end
		end
   end,
   mnesia:activity(transaction, F, [], mnesia_frag).

channel_user_blocking_status(ChannelId, UserId, IsBlocked) ->
    case mnesia:read(contact, UserId, write) of
	[] ->
	    ok;
	[Contact] ->
	    case lists:keytake(ChannelId, #contact_channel.channel_id, Contact#contact.channels) of
		{value, ContactChannel, RestChannels} ->
		    NewChannels = [ContactChannel#contact_channel{is_blocked = IsBlocked} |RestChannels],
		    mnesia:write(Contact#contact{channels = NewChannels});
		false ->
		    if
			IsBlocked -> 
			    NewChannels = [#contact_channel{channel_id = ChannelId, is_blocked = IsBlocked, is_deleted = true} | Contact#contact.channels],
			    mnesia:write(Contact#contact{channels = NewChannels});
			true ->
			    ok
		    end		    
	    end
    end.

obscene_channel_message(ModeratorId, ChannelId, MessageId) ->
    F = fun() ->
		case mnesia:read(channel_message, ChannelId, write) of
		    [] ->
			ok;
		    [ChannelMessage] ->
			IsModerator = is_moderator(ChannelId, ModeratorId),
			case lists:keytake(MessageId, #channel_message_item.message_id, ChannelMessage#channel_message.messages) of
			    {value, Message, RestMessages} ->
				ObsceneCount = length(Message#channel_message_item.obscene_count),
				if 
				    IsModerator orelse ObsceneCount >= ?OBSCENE_CHANNEL_MESSAGE_LIMIT ->
					mnesia:write(ChannelMessage#channel_message{messages = RestMessages,
										    message_count = ChannelMessage#channel_message.message_count - 1}),
					mnesia:dirty_write(#art_message{message_id = MessageId, author_id = Message#channel_message_item.author_id,
									send_date = ?datetime:now(), delivery_state = ?delivered}),
					ok;
				    true ->
					case lists:member(ModeratorId, Message#channel_message_item.obscene_count) of
					    false ->
						mnesia:write(ChannelMessage#channel_message{messages = 
					          lists:keyreplace(Message#channel_message_item.message_id, 
							     #channel_message_item.message_id, 
							     ChannelMessage#channel_message.messages, 
							     Message#channel_message_item{obscene_count = 
											  [ModeratorId | Message#channel_message_item.obscene_count]})});
					    true ->
						ok
					end
				end,
				if
				    IsModerator ->
					channel_user_blocking_status(ChannelId, Message#channel_message_item.author_id, true);
				    true ->
					ok
				end;
			    false ->
				ok
			end			
		end
    end,
    mnesia:activity(transaction, F, [], mnesia_frag).

delete_message(UserId, ContactId, MessageType, MessageId) ->
        F = fun() ->
		case MessageType of
		    ?channel ->
			case mnesia:read(channel_message, ContactId, write) of
			    [] ->
				ok;
			    [ChannelMessage] ->
				IsModerator = is_moderator(ContactId, UserId),
				if 
				    IsModerator ->
					case lists:keytake(MessageId, #channel_message_item.message_id, ChannelMessage#channel_message.messages) of
					    {value, Message, RestMessages} ->
						mnesia:write(ChannelMessage#channel_message{messages = RestMessages,
										    message_count = ChannelMessage#channel_message.message_count - 1}),
						mnesia:dirty_write(#art_message{message_id = MessageId, 
										author_id = Message#channel_message_item.author_id,
										send_date = ?datetime:now(), delivery_state = ?delivered});
					    false ->
						ok
					end,
					% notification
					spawn(?MODULE, notify_by_channel, [ContactId, ?new_command, 
									   #delete_message_command{contact_id = ContactId, message_id = MessageId}]),
					ok;
				    true ->
					?forbidden
				end
			end;
		    _OtherType ->
			?forbidden %reserved for future
		end
        end,
    mnesia:activity(transaction, F, [], mnesia_frag).

change_moderator_channel_list(ModeratorId, ChannelId, IsAdded) ->
    F = fun() ->
		case mnesia:read(user_account_data, ModeratorId, read) of
		    [] ->
			account_is_not_exist;
		    [Moderator] ->
			case mnesia:read(channel, ChannelId, write) of
			    [] ->
				channel_is_not_exist;
			    [Channel] ->
				if
				    IsAdded ->
					case lists:keymember(ModeratorId, 1, Channel#channel.moderators) of
					    true ->
						ok;
					    false ->
						mnesia:write(Channel#channel{moderators = 
								     [{ModeratorId, Moderator#user_account_data.login} | Channel#channel.moderators]})
					end;
				    true ->
					mnesia:write(Channel#channel{moderators = lists:keydelete(ModeratorId, 1, Channel#channel.moderators)})
				end,
				case mnesia:read(channel_moderator, ModeratorId, read) of
				    [] ->
					if
					    IsAdded ->
						mnesia:write(#channel_moderator{moderator_id = ModeratorId, channels = [ChannelId]});
					    true ->
						ok
					end;
				    [ChannelModerator] ->
					if 
					    IsAdded ->
						case lists:member(ChannelId, ChannelModerator#channel_moderator.channels) of
						    true ->
							ok;
						    false ->
							mnesia:write(ChannelModerator#channel_moderator{channels = 
										[ChannelId | ChannelModerator#channel_moderator.channels]})
						end;
					    true ->
						mnesia:write(ChannelModerator#channel_moderator{channels = 
										lists:delete(ChannelId, ChannelModerator#channel_moderator.channels)})
					end
				end
			end
		end
       end,
    mnesia:activity(transaction, F, [], mnesia_frag).

hide_moodstrip(ModeratorId, MoodstripId) ->
    F = fun() ->
		case mnesia:read(user_account_data, ModeratorId, read) of
		    [] ->
			?forbidden;
		    [Moderator] ->
			if 
			    Moderator#user_account_data.role == ?admin ->
				case mnesia:read(moodstrip, MoodstripId, write) of
				    [] ->
					ok;
				    [Moodstrip] ->
					mnesia:write(Moodstrip#moodstrip{is_hidden = true}),
					mnesia:delete({moodstrip_sorted, Moodstrip#moodstrip.send_date}),
					mnesia:delete({moodstrip_sorted_ru, Moodstrip#moodstrip.send_date})					
				end;
			    true ->
				?forbidden
			end
		end
       end,
    mnesia:activity(transaction, F, [], mnesia_frag).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions

get_next_channel_message_internal(_LastMessageId, _PreviousMessage, _PreviousPreviousMessage, [], _SkipLastMessage) ->
    undefined;
get_next_channel_message_internal(LastMessageId, PreviousMessage, PreviousPreviousMessage, [HeadMessage | RestMessages], SkipLastMessage) ->
    if 
	LastMessageId >= HeadMessage#channel_message_item.message_id ->
	    if 
		SkipLastMessage == true ->
		    PreviousPreviousMessage;
		true ->
		    PreviousMessage
	    end;
	true ->
	    get_next_channel_message_internal(LastMessageId, HeadMessage, PreviousMessage, RestMessages, SkipLastMessage)
    end.

get_start_key(Table, RecordsPerPage, DateTimeMin, RecordCount, CurrentCount, Key, PreviousKey) ->
    if
	CurrentCount < RecordCount ->
	    NewKey = mnesia:prev(Table, Key),
	    if 
		NewKey =/= '$end_of_table' andalso NewKey > DateTimeMin ->
		    if
			(CurrentCount + 1) rem RecordsPerPage == 0 ->
			    NewPreviousKey = NewKey;
			true ->
			    NewPreviousKey = PreviousKey
		    end,
		    get_start_key(Table, RecordsPerPage, DateTimeMin, RecordCount, CurrentCount + 1, NewKey, NewPreviousKey);
		true ->
		    {(CurrentCount div RecordsPerPage) + 1, PreviousKey}
	    end;
	true ->
	    {(CurrentCount div RecordsPerPage) + 1, Key}
    end.

get_sorted_user_accounts(DateTimeMin, RecordCount, CurrentCount, Key, Result) ->
    if
	CurrentCount < RecordCount ->
	    case mnesia:read(user_account_sorted, Key) of
		[] ->
		    NewResult = Result,
		    NewCount = CurrentCount;
		[UserAccountSorted] ->
		    case mnesia:read(user_account_data, UserAccountSorted#user_account_sorted.id) of
			[] ->
			    NewResult = Result,
			    NewCount = CurrentCount;
			[U] ->
			    NewResult = [#user_info{user_id = U#user_account_data.id, login = U#user_account_data.login,
						    name = U#user_account_data.name, creation_date = U#user_account_data.creation_date, 
						    motto = U#user_account_data.motto, country = U#user_account_data.country,
						    city = U#user_account_data.city, sex = U#user_account_data.sex,
						    birth_day = U#user_account_data.birth_day,
						    userpic_url = ?repository:get_url(?USERPIC_PREFIX, U#user_account_data.id)} | Result],
			    NewCount = CurrentCount + 1
		    end
	    end,		    
	    NewKey = mnesia:prev(user_account_sorted, Key),
	    if 
		NewKey =/= '$end_of_table' andalso NewKey > DateTimeMin ->
		    get_sorted_user_accounts(DateTimeMin, RecordCount, NewCount, NewKey, NewResult);
		true ->
		    {false, lists:reverse(NewResult)}
	    end;
	true ->
	    {true, lists:reverse(Result)}
    end.

get_sorted_moodstrips(DateTimeMin, RecordCount, CurrentCount, Key, Result, SortedTable) ->
    if
	CurrentCount < RecordCount ->
	    case mnesia:read(SortedTable, Key) of
		[] ->
		    NewResult = Result,
		    NewCount = CurrentCount;
		[{SortedTable, _CreationDate, Id}] ->
		    case mnesia:read(moodstrip, Id) of
			[] ->
			    NewResult = Result,
			    NewCount = CurrentCount;
			[M] ->
			    NewResult = [#moodstrip_result{moodstrip_id = M#moodstrip.id, author_id = M#moodstrip.author_id, 
							   author_login = M#moodstrip.author_login,
							   title = M#moodstrip.title, send_date = M#moodstrip.send_date, 
							   url = get_moodstrip_url(M#moodstrip.items),
							   count = ?string:len(M#moodstrip.items)} | Result],
			    NewCount = CurrentCount + 1
		    end
	    end,		    
	    NewKey = mnesia:prev(SortedTable, Key),
	    if 
		NewKey =/= '$end_of_table' andalso NewKey > DateTimeMin ->
		    get_sorted_moodstrips(DateTimeMin, RecordCount, NewCount, NewKey, NewResult, SortedTable);
		true ->
		    {false, lists:reverse(NewResult)}
	    end;
	true ->
	    {true, lists:reverse(Result)}
    end.

publish_moodstrip_internal(MoodStrip, SortedTable) ->
    mnesia:write(MoodStrip#moodstrip{is_published = true, items = lists:reverse(MoodStrip#moodstrip.items)}),
    if 
	MoodStrip#moodstrip.is_hidden == false ->
	    MoodstripSorted = setelement(1, #moodstrip_sorted{creation_date = MoodStrip#moodstrip.send_date, 
							      id = MoodStrip#moodstrip.id}, SortedTable),
	    mnesia:write(MoodstripSorted),					
	    % delete unnecessary records from sorted table
	    Count = mnesia:table_info(SortedTable, size),
	    if 
		Count > ?MAX_SORTED_RECORDS_COUNT * 1.2 ->
		    spawn(?MODULE, delete_old_records, [SortedTable, ?MAX_SORTED_RECORDS_COUNT]);
		true ->
		    ok
	    end,
	    ok;
	true ->
	    ok
    end.

delete_old_records(Table, MaxCount) ->
    CurrentCount = mnesia:table_info(Table, size),
    if 
	CurrentCount > MaxCount ->
	    Key = mnesia:dirty_first(Table),
	    mnesia:dirty_delete(Table, Key),
	    delete_old_records(Table, MaxCount);
	true ->
	    ok
    end.

skip_records(QC, RecordType, 0, RecordsPerPage, RealPageNumber, PreviousResult) ->
    Result = qlc:next_answers(QC, RecordsPerPage),
    if 
	Result == [] -> 
	    {RecordType, RealPageNumber - 1, false, PreviousResult};
	true ->
	    case qlc:next_answers(QC, 1) of
		[] ->
		    HasMore = false;
		_Other ->
		    HasMore = true
	    end,
	    {RecordType, RealPageNumber, HasMore, Result}
    end;
skip_records(QC, RecordType, PageNumber, RecordsPerPage, RealPageNumber, PreviousResult) ->
    case qlc:next_answers(QC, RecordsPerPage) of
	[] ->
	    {RecordType, RealPageNumber - 1, false, PreviousResult};
	NewPreviousResult ->
	    skip_records(QC, RecordType, PageNumber - 1, RecordsPerPage, RealPageNumber + 1, NewPreviousResult)
    end.

add_artmessage_in_queues([], _MessageID, _Type, _AuthorId, _SendDate) ->
    ok;
add_artmessage_in_queues([RecipientId | Rest], MessageId, Type, AuthorId, SendDate) ->
    % Type: friends, private, group ('group' is for future)
    case mnesia:read(delivery, RecipientId, write) of
	[] ->
	    Counts = case Type of
			 friends ->
			     {1, 0};
			 private ->
			     {0, 1}
		     end,
	    mnesia:write(#delivery{recipient_id = RecipientId, count = Counts,
					      message_ids = [#queue_item{message_id = MessageId, type = Type, author_id = AuthorId, 
									   send_date = SendDate}]});
	[#delivery{message_ids = OldMessages, count = {FriendsCount, PrivateCount}} = Queue] ->
	    if 
		Type == private andalso PrivateCount >= ?MaxPrivateMessagesInQueue ->
		    delete_recipient_from_artmessage_table(RecipientId, MessageId),
		    ok; % @todo report failure
		true ->
		    {Counts, Messages} = case Type of
					     friends ->
						 if
						     FriendsCount < ?MaxFriendsMessagesInQueue ->
							 {{FriendsCount + 1, PrivateCount}, OldMessages};
						     true ->
							 {RemovedMessage, RestMessages} = remove_oldest_friends_message(OldMessages),
							 case RemovedMessage of
							     #queue_item{message_id = RemovedMessageId} ->
								 delete_recipient_from_artmessage_table(RecipientId, RemovedMessageId);
							     undefined ->
								 ok
							 end,
							 {{FriendsCount, PrivateCount}, RestMessages}
						 end;
					     private ->
						 {{FriendsCount, PrivateCount + 1}, OldMessages}
			     end,
		    mnesia:write(Queue#delivery{count = Counts,
						message_ids = [#queue_item{message_id = MessageId, type = Type, author_id = AuthorId, 
									   send_date = SendDate} | Messages]})
	    end
    end,
    add_artmessage_in_queues(Rest, MessageId, Type, AuthorId, SendDate).

remove_oldest_friends_message(List) ->
    remove_first_friends_message(lists:reverse(List)).
remove_first_friends_message(List) ->
    remove_first_friends_message(List, undefined, []).
remove_first_friends_message([], RemovedItem, Result) ->
    {RemovedItem, Result};
remove_first_friends_message([#queue_item{type = friends} = Item | Rest], undefined = _RemovedItem, Result) ->
    remove_first_friends_message(Rest, Item, Result);
remove_first_friends_message([Item | Rest], RemovedItem, Result) ->
    remove_first_friends_message(Rest, RemovedItem, [Item | Result]).

get_user_contact_ids(UserId) ->
    case mnesia:read(contact, UserId, read) of
	[] ->
	    [];
	[Contact] ->
	    [C#contact_item.contact_id || C <- Contact#contact.items, C#contact_item.authorization_state == ?authorized]
    end.

get_moodstrip_url([]) ->
    undefined;
get_moodstrip_url([Head | _Rest]) ->
    ?repository:get_url(?MOODSTRIP_PREFIX, Head#moodstrip_item.image_id).

delivery_artmessage(MessageId, AuthorId, Type) ->
    F = fun() ->
		case mnesia:read(art_message, MessageId, write) of
		    [] ->
			ok;
		    [ArtMessage] ->
			mnesia:write(ArtMessage#art_message{delivery_state = ?to_be_delivered}),
			add_artmessage_in_queues(ArtMessage#art_message.recepient_ids, MessageId, Type, AuthorId, ArtMessage#art_message.send_date)
		end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).

delivery_channel_message(ChannelId, MessageId, AuthorId, SendDate) ->
    F = fun() ->
	   case mnesia:read(art_message, MessageId, write) of
	       [] ->
		   ?contact_not_found;
	       [ArtMessage] ->
		   case mnesia:dirty_read(user_account_data, AuthorId) of
		       [] ->
			   ?contact_not_found;
		       [User] ->
			   mnesia:write(ArtMessage#art_message{delivery_state = ?channel_delivered}),
			   MessageItem = #channel_message_item{message_id = MessageId,
							       author_id = AuthorId,
							       author_login = User#user_account_data.login,
							       send_date = SendDate},
			   case mnesia:read(channel_message, ChannelId, write) of
			       [] ->
				   mnesia:write(#channel_message{channel_id = ChannelId,
								 messages = [MessageItem],
								 message_count = 1
								});
			       [ChannelMessage] ->
				   if 
				       ChannelMessage#channel_message.message_count > ?CHANNEL_MESSAGE_QUEUE_LENGTH - 1 ->
					   {MessageList, RestMessageList} = lists:split(?CHANNEL_MESSAGE_QUEUE_LENGTH,
											[MessageItem | ChannelMessage#channel_message.messages]),
					   MessageCount = ?CHANNEL_MESSAGE_QUEUE_LENGTH,
					   spawn(?MODULE, delete_channel_messages, [RestMessageList]);
				       true ->
					   MessageList = [MessageItem | ChannelMessage#channel_message.messages],
					   MessageCount = ChannelMessage#channel_message.message_count + 1
				   end,
				   mnesia:write(ChannelMessage#channel_message{messages = MessageList, message_count = MessageCount})
			   end
		   end
	   end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag).

delete_channel_messages([]) ->
    ok;
delete_channel_messages([#channel_message_item{send_date = SendDate, author_id = AuthorId, message_id = MessageId} | RestChannelMessages]) ->
    mnesia:dirty_write(#art_message{message_id = MessageId, author_id = AuthorId, send_date = SendDate, delivery_state = ?delivered}),
    delete_channel_messages(RestChannelMessages).

delete_recipient_from_artmessage_table(RecipientId, MessageId) ->
    case mnesia:read(art_message, MessageId, write) of
	[] ->
	    ok;
	[ArtMessageRecord] ->
	    Recepient_ids = lists:delete(RecipientId, ArtMessageRecord#art_message.recepient_ids),
	    if 
		Recepient_ids == [] ->
		    mnesia:write(ArtMessageRecord#art_message{delivery_state = ?delivered, recepient_ids = Recepient_ids});
		true ->
		    mnesia:write(ArtMessageRecord#art_message{recepient_ids = Recepient_ids})
	    end
    end.

is_moderator(ChannelId, ModeratorId) ->
    case mnesia:read(user_account_data, ModeratorId, read) of
	[] ->
	    false;
	[User] ->
	    if
		User#user_account_data.role == ?admin ->
		    true;
		true ->
		    case mnesia:read(channel_moderator, ModeratorId, read) of
			[] ->
			    false;
			[ChannelModerator] ->
			    lists:member(ChannelId, ChannelModerator#channel_moderator.channels)
		    end
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% exceptions handling

get_exception_message(?exception_data(#cannot_select_notification_server{})) ->
    io_lib:format("Can't select notification server", []).
