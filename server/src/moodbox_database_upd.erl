-module(moodbox_database_upd).

%%
%% FUNCTIONS FOR DATABASE UPDATE
%%
%% transforms 'delivery' table by adding new field (see delivery_new) and transforming it's records
%%

-export([update_delivery/0, update_contact/0, update_moodstrip/0, update_channel_message/0, update_channel/0, update_artmessage/0]).

-record(delivery, {recipient_id, count = {0, 0}, message_ids = [], last_got_author_id}).
-record(delivery_new, {recipient_id, count = {0, 0}, message_ids = [], last_got_author_id, commands = []}).

-record(contact, {owner_id, items = []}).
-record(contact_new, {owner_id, items = [], channels = []}).
-record(contact_item, {contact_id, message, authorization_state, is_blocked = false}).
-record(contact_channel, {channel_id, is_blocked = false, is_deleted = false}).

-record(moodstrip, {id, author_id, author_login, send_date, title = "", upper_title = "", is_published = false, items = [], is_blocked}).
-record(moodstrip_new, {id, author_id, author_login, send_date, title = "", upper_title = "", is_published = false, items = [], language, 
			is_hidden, channel_id}).

-record(channel_message, {channel_id, messages = [], message_count}).
-record(channel_message_item, {message_id, author_id, author_login, send_date, obscene_count = []}).

-record(channel, {id, author_id, creation_date, title, upper_title, short_description, 
		  full_description, users = [], user_count = 0, order = 0, order_ru = 0}).
-record(channel_new, {id, author_id, creation_date, title, upper_title, short_description, 
		  full_description, users = [], user_count = 0, order = 0, order_ru = 0, moderators}).
-record(channel_moderator, {moderator_id, channels = []}).
-record(user_account_data, {id, login, upper_login = "", password, creation_date, is_logon_allowed = true, motto = "", about_me = "", name = "", 
			    upper_name = "", country, city = "", email = "", upper_email = "", sex, birth_day, language, allow_news, allow_publishing, 
			    allow_show_friends = true, role}).

-record(art_message, {message_id, send_date, recepient_ids = [], delivery_state, author_id}).
-record(art_message_new, {message_id, send_date, recepient_ids = [], delivery_state, author_id, is_public = false}).

update_delivery() ->
    mnesia:transform_table(delivery, fun transform_delivery/1, record_info(fields, delivery_new)).

update_contact() ->
    mnesia:transform_table(contact, fun transform_contact/1, record_info(fields, contact_new)).

update_moodstrip() ->
    mnesia:transform_table(moodstrip, fun transform_moodstrip/1, record_info(fields, moodstrip_new)).

update_channel_message() ->
    mnesia:transform_table(channel_message, fun transform_channel_message/1, record_info(fields, channel_message)).

update_channel() ->
    mnesia:transform_table(channel, fun transform_channel/1, record_info(fields, channel_new)).

update_artmessage() ->
    mnesia:transform_table(art_message, fun transform_artmessage/1, record_info(fields, art_message_new)).

transform_delivery(#delivery{recipient_id = RecipientId, count = Counts, message_ids = MessageIds, last_got_author_id = LastGotAuthorId}) ->
    Result = #delivery_new{recipient_id = RecipientId, count = Counts, message_ids = MessageIds, last_got_author_id = LastGotAuthorId},
    setelement(1, Result, delivery).

transform_contact(#contact{owner_id = OwnerId, items = Items}) ->
    case lists:keytake(1, #contact_item.contact_id, Items) of
	{value, _Contact, RestContacts} ->
	    case mnesia:dirty_read(channel, 1) of
		[] ->
		    mnesia:dirty_write(#channel{id = 1, users = [OwnerId], user_count = 1, author_id = 1, creation_date = moodbox_datetime:now(), 
						title = "Newbies", upper_title = "NEWBIES", short_description = "Channel for newbies", 
						order = 0, order_ru = 1000});
		[Channel] ->
		    mnesia:dirty_write(Channel#channel{users = [OwnerId | Channel#channel.users], user_count = Channel#channel.user_count + 1})
	    end,
	    Result = #contact_new{owner_id = OwnerId, items = RestContacts, channels = [#contact_channel{channel_id = 1}]};
	false ->
	    Result = #contact_new{owner_id = OwnerId, items = Items, channels = []}
    end,
    setelement(1, Result, contact).

transform_moodstrip(#moodstrip{id =Id, author_id = AuthorId, author_login = AuthorLogin, send_date = SendDate, title = Title, 
			       upper_title = UpperTitle, is_published = IsPublished, items = Items, is_blocked = _IsBlocked}) ->
    Result = #moodstrip_new{id =Id, author_id = AuthorId, author_login = AuthorLogin, send_date = SendDate, title = Title, 
			       upper_title = UpperTitle, is_published = IsPublished, items = Items, is_hidden = false},
    setelement(1, Result, moodstrip).

transform_channel_message(#channel_message{channel_id = ChannelId, messages = Messages, message_count = MessageCount}) ->
    Result = #channel_message{channel_id = ChannelId, 
			      messages = lists:map(fun(MessageItem) ->
							   {channel_message_item, MessageId, AuthorId, AuthorLogin, SendDate, _ObsceneCount} = MessageItem,
							   #channel_message_item{message_id = MessageId,  author_id = AuthorId, 
										 author_login = AuthorLogin, send_date = SendDate}
						   end, Messages),
			      message_count = MessageCount},
    setelement(1, Result, channel_message).

transform_channel(#channel{id =Id, author_id = AuthorId, creation_date = CreationDate, title = Title, upper_title = UpperTitle, 
			   short_description = ShortDescription, full_description = FullDescription, users = Users, user_count = UserCount, 
			   order = Order, order_ru = OrderRu}) ->
    Result = #channel_new{id =Id, author_id = AuthorId, creation_date = CreationDate, title = Title, upper_title = UpperTitle, 
			   short_description = ShortDescription, full_description = FullDescription, users = Users, user_count = UserCount, 
			   order = if Order == 100 -> 1000; true -> Order end, order_ru = if OrderRu == 100 -> 1000; true -> OrderRu end,
			   moderators = get_channel_moderators(Id, mnesia:dirty_all_keys(channel_moderator), [])
			 },
    setelement(1, Result, channel).

transform_artmessage(#art_message{message_id = Id, send_date = SendDate, recepient_ids = RecepientIds, delivery_state = DeliveryState,
				  author_id = AuthorId}) ->
    Result = #art_message_new{message_id = Id, send_date = SendDate, recepient_ids = RecepientIds, delivery_state = DeliveryState, 
			      author_id = AuthorId},
    setelement(1, Result, art_message).

get_channel_moderators(_ChannelId, [], Moderators) ->
    Moderators;
get_channel_moderators(ChannelId, [ModeratorId | RestModeratorIds], Moderators) ->
    [Moderator] = mnesia:dirty_read(channel_moderator, ModeratorId),
    case lists:member(ChannelId, Moderator#channel_moderator.channels) of
	true ->
	    [User] = mnesia:dirty_read(user_account_data, ModeratorId),
	    get_channel_moderators(ChannelId, RestModeratorIds, [{ModeratorId, User#user_account_data.login} | Moderators]);
	false ->
	    get_channel_moderators(ChannelId, RestModeratorIds, Moderators)
    end.

