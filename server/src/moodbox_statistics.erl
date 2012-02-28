-module(moodbox_statistics).

-export([get_statistics/0]).
-export([user_count/1, drawing_count/1, using_service/3, active_users/2]).

-include("moodbox_facade.hrl").

-include_lib("stdlib/include/qlc.hrl").

-define(SERVER_STATISTICS_TEMPLATE, "priv/templates/server_statistics_template.html").

get_statistics() ->
    {ok, Template} = file:read_file(?SERVER_STATISTICS_TEMPLATE),
    [{sequence,art_message, ArtMessageCount}] = mnesia:dirty_read(sequence, art_message),
    io_lib:format(Template, [mnesia:table_info(session, size), mnesia:table_info(user_account_data, size),
			     ArtMessageCount, mnesia:table_info(moodstrip, size)]).

user_count(DateTime) ->
    F = fun() ->
		UserIds = qlc:e(qlc:q([U#user_account_data.id || 
				       U <- mnesia:table(user_account_data),
				       U#user_account_data.creation_date =< moodbox_datetime:from_erlang_datetime(DateTime)])),
		length(UserIds)
    end,
    mnesia:activity(async_dirty, F, []).

drawing_count(DateTime) ->
    F = fun() ->
		Ids = qlc:e(qlc:q([A#art_message.message_id || 
				   A <- mnesia:table(art_message),
				   A#art_message.send_date =< moodbox_datetime:from_erlang_datetime(DateTime)])),
		length(Ids)
    end,
    mnesia:activity(async_dirty, F, []).

using_service(DateTime, UserCount, Period) ->
    F = fun() ->
		QH = qlc:q([{U#user_account_data.id, U#user_account_data.creation_date} || 
				 U <- mnesia:table(user_account_data),
				 U#user_account_data.creation_date > moodbox_datetime:from_erlang_datetime(DateTime)]),
		QC = qlc:cursor(QH),
		Users = qlc:next_answers(QC, UserCount),
		qlc:delete_cursor(QC),
		Channels = qlc:e(qlc:q([{C#channel.id, moodbox_datetime:from_erlang_datetime(DateTime)} || 
			      C <- mnesia:table(channel)])),

		{drawing_count(Users, Period, 0) / UserCount, drawing_count(Channels, Period, 0)}
		
    end,
    mnesia:activity(async_dirty, F, []).

drawing_count([], _Period, DrawingAmount) ->
    DrawingAmount;
drawing_count([{ContactId, CreationDate} | Rest], Period, DrawingAmount) ->
    Ids = qlc:e(qlc:q([A#art_message.message_id ||
	   A <- mnesia:table(art_message),
	   A#art_message.send_date >= CreationDate + Period - 518400000000 andalso % 1 week = 518400000000, 2 weeks = 1123200000000, 4 weeks=2332800000000
           A#art_message.send_date =< CreationDate + Period andalso % 5 weeks = 2937600000000
           A#art_message.author_id == ContactId])), 
    drawing_count(Rest, Period, DrawingAmount + length(Ids)).

active_users(StartDateTime, EndDateTime) ->
    F = fun() ->
		Messages = qlc:e(qlc:q([A#art_message.author_id ||
				        A <- mnesia:table(art_message),
				        A#art_message.send_date >= moodbox_datetime:from_erlang_datetime(StartDateTime) andalso
				        A#art_message.send_date =< moodbox_datetime:from_erlang_datetime(EndDateTime)])),
		Dict = dict:new(),
		% SortedList = 
		lists:sort(fun({_User1Id, A1}, {_User2Id, A2}) -> A1 >= A2 end, dict:to_list(drawing_count_per_contact(Messages, Dict)))
		
    end,
    mnesia:activity(async_dirty, F, []).

drawing_count_per_contact([], Dict) ->
    Dict;
drawing_count_per_contact([UserId | Rest], Dict) ->
    drawing_count_per_contact(Rest, dict:update_counter(UserId, 1, Dict)).

%dict:fold(fun(ServerPid, Users, _) -> 
    %				     moodbox_notification:add_channel_notification(ServerPid, Users, ChannelId, Event)
    %			     end, 
    %			     ok, notify_by_channel_internal(Channel#channel.users, Dict, Command))
