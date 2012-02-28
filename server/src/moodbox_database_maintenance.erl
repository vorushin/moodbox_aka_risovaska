-module(moodbox_database_maintenance).

-export([fill_sorted_tables/0, update_upper_case_fields/0, delete_unnecessary_user_account/1, delete_moodstrips_by_user/1, 
	 decrease_moodstrip_creation_date/2, add_newbie_channel_to_all/0, delete_unnecessary_art_messages/1,
	 check_contacts/0, delete_old_channel_messages/0, check_login_dublicates/0, get_all_emails/0
	]).

-include("moodbox_facade.hrl").
-include("moodbox_repository.hrl").
-include("moodbox_config.hrl").

-include_lib("stdlib/include/qlc.hrl").

check_contacts() ->
    F = fun() -> 
 	       Result = qlc:e(qlc:q([C ||
				 C <- mnesia:table(contact)])),
	       [mnesia:write(Contact#contact{channels = check_contact_channels(Contact#contact.channels, Contact#contact.channels)}) || Contact <- Result]
   end,
   mnesia:activity(async_dirty, F, [], mnesia_frag).

check_contact_channels([], Channels) ->
    Channels;
check_contact_channels([Channel | RestChannels], Channels) ->
    case lists:foldl(fun(Item, Count) -> 
			     if 
				 Item#contact_channel.channel_id == Channel#contact_channel.channel_id ->
				     Count + 1;
				 true ->
				     Count
			     end
		     end, 
		     0, Channels) of
	0 ->
	    check_contact_channels(RestChannels, Channels);
	1 ->
	    check_contact_channels(RestChannels, Channels);
	_CountOfDublicates ->
	    NewChannels = lists:keydelete(Channel#contact_channel.channel_id, #contact_channel.channel_id, Channels),
	    io:format("New channels without dublicate: ~p!~n", [NewChannels]),
	    check_contact_channels(RestChannels, NewChannels)
    end.

% run carefully, will be slow on large data, can even crash system
fill_sorted_tables() ->
    fill_sorted_moodstrip_table(?MAX_SORTED_RECORDS_COUNT, moodstrip_sorted, undefined),
    fill_sorted_moodstrip_table(?MAX_SORTED_RECORDS_COUNT, moodstrip_sorted_ru, ru),
    fill_sorted_user_account_table(?MAX_SORTED_RECORDS_COUNT).

fill_sorted_moodstrip_table(Count, SortedTable, Language) ->
    F = fun() -> 
		QH = qlc:keysort(2,
				 qlc:q([{SortedTable, M#moodstrip.send_date, M#moodstrip.id} || 
					   M <- mnesia:table(moodstrip),
					   M#moodstrip.is_published == true andalso M#moodstrip.language == Language
				       ]),
				{order, descending}),
 	       QC = qlc:cursor(QH),
 	       Result = qlc:next_answers(QC, Count),
 	       qlc:delete_cursor(QC),
	       [mnesia:write(X) || X <- Result]
   end,
   mnesia:activity(async_dirty, F, [], mnesia_frag).

fill_sorted_user_account_table(Count) ->
    F = fun() -> 
 	       QH = qlc:keysort(2,
				qlc:q([{user_account_sorted, U#user_account_data.creation_date, U#user_account_data.id} || 
					U <- mnesia:table(user_account_data)
				      ]),
				{order, descending}),
 	       QC = qlc:cursor(QH),
 	       Result = qlc:next_answers(QC, Count),
 	       qlc:delete_cursor(QC),
	       [mnesia:write(X) || X <- Result]
   end,
   mnesia:activity(async_dirty, F, [], mnesia_frag).
                            
update_upper_case_fields() ->
    F = fun() -> 
		qlc:e(qlc:q([mnesia:write(User#user_account_data{upper_login = moodbox_string:to_upper(User#user_account_data.login),
								 upper_name = moodbox_string:to_upper(User#user_account_data.name),
								 upper_email = moodbox_string:to_upper(User#user_account_data.email)}) || 
				User <- mnesia:table(user_account_data)])),
		qlc:e(qlc:q([mnesia:write(Moodstrip#moodstrip{upper_title = moodbox_string:to_upper(Moodstrip#moodstrip.title)}) || 
				Moodstrip <- mnesia:table(moodstrip)]))
	end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

delete_unnecessary_user_account(Login) ->
    F = fun() ->
		case mnesia:index_read(user_account_data, Login, #user_account_data.login) of
		    [] ->
			not_exists;
		    [User] ->
			ok = mnesia:delete({user_account_data, User#user_account_data.id}),
			Keys = qlc:e(qlc:q([U#user_account_sorted.creation_date || U <- mnesia:table(user_account_sorted), 
				     U#user_account_sorted.id == User#user_account_data.id])),
			[mnesia:delete({user_account_sorted, Key}) || Key <- Keys],
			ok = mnesia:delete({contact, User#user_account_data.id}),
			ok = mnesia:delete({delivery, User#user_account_data.id}),
			ok = mnesia:delete({logon_info, User#user_account_data.id}),
			ok = mnesia:delete({notification_error, User#user_account_data.id}),
			ok = mnesia:delete({user_password_reset_request, User#user_account_data.id}),
			MoodstripIds = qlc:e(qlc:q([M#moodstrip.id || M <- mnesia:table(moodstrip),
						    M#moodstrip.author_id == User#user_account_data.id])),
			[delete_moodstrip_complitely(MoodstripId) || MoodstripId <- MoodstripIds]			
		end
    end,
    mnesia:activity(transaction, F, [], mnesia_frag).

delete_moodstrip_complitely(MoodstripId) ->
    moodbox_repository:delete_file(?MOODSTRIP_PREFIX, integer_to_list(MoodstripId)),
    Keys = qlc:e(qlc:q([M#moodstrip_sorted.creation_date || M <- mnesia:table(moodstrip_sorted), 
				     M#moodstrip_sorted.id == MoodstripId])),
    [mnesia:delete({moodstrip_sorted, Key}) || Key <- Keys],
    Keys_ru = qlc:e(qlc:q([M#moodstrip_sorted.creation_date || M <- mnesia:table(moodstrip_sorted_ru), 
				     M#moodstrip_sorted.id == MoodstripId])),
    [mnesia:delete({moodstrip_sorted_ru, Key}) || Key <- Keys_ru],
    mnesia:delete({moodstrip, MoodstripId}).

delete_moodstrips_by_user(UserId) ->
    F = fun() ->
		MoodstripIds = qlc:e(qlc:q([M#moodstrip.id || M <- mnesia:table(moodstrip),
							      M#moodstrip.author_id == UserId])),
		[delete_moodstrip_complitely(MoodstripId) || MoodstripId <- MoodstripIds]
    end,
    mnesia:activity(transaction, F, [], mnesia_frag).

decrease_moodstrip_creation_date(MoodStripId, CountOfDays) ->
    F = fun() ->
		case mnesia:read(moodstrip, MoodStripId, write) of
		    [] ->
			not_exists;
		    [Moodstrip] ->
			mnesia:delete({moodstrip_sorted, Moodstrip#moodstrip.send_date}),
			NewSendDate = Moodstrip#moodstrip.send_date - moodbox_datetime:from_erlang_datetime({{1970, 1, CountOfDays}, {0,0,0}}),
			mnesia:write(Moodstrip#moodstrip{send_date = NewSendDate}),
			mnesia:write(#moodstrip_sorted{creation_date = NewSendDate, id = MoodStripId})
		end
    end,
    mnesia:activity(transaction, F, [], mnesia_frag).

add_newbie_channel_to_all() ->
    F = fun() ->
		qlc:e(qlc:q([if 
		     U#user_account_data.id == ?NewbieChannelId ->
			 ok;
		     true ->
			 case mnesia:dirty_read(contact, U#user_account_data.id) of 
			     [] ->
				 case mnesia:read(contact, ?NewbieChannelId, write) of
				     [] ->
					 ok;
				     [NewbieCommunity] ->
					 mnesia:write(#contact{owner_id = U#user_account_data.id, 
							       items = [#contact_item{contact_id = ?NewbieChannelId, 
										      authorization_state = ?authorized}] }),
					 mnesia:write(NewbieCommunity#contact{items = [#contact_item{contact_id = U#user_account_data.id,
												     authorization_state = ?authorized} | 
										       NewbieCommunity#contact.items]})
				 end;
			     [Contact] ->
				 case lists:keymember(?NewbieChannelId, #contact_item.contact_id, Contact#contact.items) of
				     true ->
					 ok;
				     false ->
					 case mnesia:read(contact, ?NewbieChannelId, write) of
					     [] ->
						 ok;
					     [NewbieCommunity] ->
						 mnesia:write(#contact{owner_id = U#user_account_data.id,
								       items = [#contact_item{contact_id = ?NewbieChannelId, 
										      authorization_state = ?authorized} | Contact#contact.items] }),
						 mnesia:write(NewbieCommunity#contact{items = [#contact_item{contact_id = U#user_account_data.id,
													     authorization_state = ?authorized} | 
											       NewbieCommunity#contact.items]})
					 end
				 end
			 end
		 end
		 || U <- mnesia:table(user_account_data)]))
    end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

delete_unnecessary_art_messages(Type) ->
    F = fun() -> 
	       QH = qlc:q([A#art_message.message_id || 
			   A <- mnesia:table(art_message),
			   A#art_message.delivery_state == Type ]),
	       QC = qlc:cursor(QH),
	       Result = qlc:next_answers(QC, 100),
	       qlc:delete_cursor(QC),
	       if 
		   Result == [] ->
		       ok;
		   true ->
		       io:format("Delete following artmessages: ~p~n", [Result]),
		       delete_art_messages(Result),
		       timer:sleep(1000),
		       delete_unnecessary_art_messages(Type)
	       end
    end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

delete_art_messages([]) ->
    ok;
delete_art_messages([MessageId | RestMessageIds]) ->
    ok = moodbox_repository:delete_file(?ARTMESSAGE_PREFIX, integer_to_list(MessageId)),
    mnesia:dirty_delete(art_message, MessageId),
    delete_art_messages(RestMessageIds).

delete_old_channel_messages() ->
    F = fun() -> 
		[case mnesia:read(channel_message, ChannelId, write) of
		     [] ->
			 ok;
		     [ChannelMessage] ->
			 case delete_channel_messages_internal(ChannelMessage#channel_message.messages, []) of
			     {NewCount, NewMessages} ->
				 mnesia:write(ChannelMessage#channel_message{messages = NewMessages, message_count = NewCount});
			     _ ->
				 error
			 end
		 end
		 || ChannelId <- mnesia:all_keys(channel_message)]
    end,
    mnesia:activity(transaction, F, [], mnesia_frag).
        
delete_channel_messages_internal([], NewMessages) ->
    {length(NewMessages), lists:reverse(NewMessages)};
delete_channel_messages_internal([Message | RestMessages], NewMessages) ->
    if
	Message#channel_message_item.message_id < 167000 ->
	    delete_channel_messages_internal(RestMessages, NewMessages);
	true ->
	    delete_channel_messages_internal(RestMessages, [Message | NewMessages])
    end.

check_login_dublicates() ->
    F = fun() -> 
		[case mnesia:dirty_read(user_account_data, UserId) of
		     [] ->
			 ok;
		     [User] ->
			 Ids = qlc:e(qlc:q([U#user_account_data.id || U <- mnesia:table(user_account_data),
								      U#user_account_data.upper_login == User#user_account_data.upper_login])),
			 if 
			     length(Ids) > 1 ->
				 io:format("Alert!!!! ~p~n", [Ids]);
			     true ->
				 ok
			 end
		 end
		 || UserId <- mnesia:all_keys(user_account_data)]
    end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

get_all_emails() ->
    F = fun() -> 
		Emails = qlc:e(qlc:q([integer_to_list(U#user_account_data.id) ++ ", " ++ U#user_account_data.email ++ [13,10]
				      || U <- mnesia:table(user_account_data),
				      U#user_account_data.allow_news == true andalso 
				      U#user_account_data.creation_date > 1233014400000000 % moodbox_datetime:from_erlang_datetime({{2009,1,27}, {0,0,0}})
				      andalso U#user_account_data.creation_date < 1235095200000000 
				     ])),
		file:write_file("emails.txt", Emails)
    end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

