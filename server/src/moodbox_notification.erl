-module(moodbox_notification).

-behavior(gen_server).

%% gen_server callbacks
-export([start_link/0, start/0, stop/0, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
%% server's API
-export([add_process/5, add_channel_notification/4, add_notification/4, add_user/3, replace_user/3, disconnect_user/2, get_loading/1]).
% internal processes
-export([add_channel_notification_internal/4, add_notification_internal/4, add_process_internal/5, create_packet/1, retry_packet/1, 
	 add_user_internal/2, replace_user_internal/2, update_user_internal/1, disconnect_user_internal/1, check_notification_errors/2, 
	 check_outdated_users/0]).

-include("moodbox_config.hrl").
-include("moodbox_facade.hrl").
-include("moodbox_context.hrl").
-include("moodbox_database.hrl").

-define(user_count, user_count).
-define(check_errors_process, check_errors_process).
-define(check_outdated_users_process, check_outdated_users_process).
-define(safe_try(Expr), try Expr catch _:_ -> undefined end).

-define(datetime, moodbox_datetime).

-record(notification_state, {user_count, check_errors_process_pid, check_outdated_users_process_pid, address}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start(?MODULE, [], []).

stop() ->
    % delete server and all users from session table register on this notification server
    moodbox_database:delete_notification_server(self()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% notification server API

add_process(ServerPID, UserId, Key, ProcessPID, PacketId) ->
    gen_server:call(ServerPID, {add_process, UserId, Key, ProcessPID, PacketId}).
   
add_notification(ServerPID, ForUsers, FromUserId, Event) ->
    gen_server:call(ServerPID, {add_notification, ForUsers, FromUserId, Event}).

add_channel_notification(ServerPID, ForUsers, FromUserId, Event) ->
    gen_server:call(ServerPID, {add_channel_notification, ForUsers, FromUserId, Event}).

add_user(ServerPID, UserId, Key) ->
    gen_server:cast(ServerPID, {add_user, UserId, Key}).

replace_user(ServerPID, UserId, Key) ->
    gen_server:cast(ServerPID, {replace_user, UserId, Key}).

disconnect_user(ServerPID, UserId) ->
    gen_server:cast(ServerPID, {disconnect_user, UserId}).

get_loading(ServerPID) ->
    gen_server:call(ServerPID, get_loading).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks functions for gen_server

init(_Args) ->
    mnesia:wait_for_tables(?NOTIFICATION_SERVER_TABLE_LIST, 60000),

    ServerAddress = get_notification_server_address(),
    moodbox_database:add_notification_server(self(), ServerAddress),
    case ets:info(notification_process) of
	undefined ->
	    ets:new(notification_process, [set, public, named_table]);
	_Other ->
	    ok
    end,
    case ets:info(notification_queue) of
	undefined ->
	    ets:new(notification_queue, [bag, public, named_table]);
	_Other2 ->
	    ok
    end,
    case ets:info(notification_packet) of
	undefined ->
	    ets:new(notification_packet, [set, public, named_table]);
	_Other3 ->
	    ok
    end,
    case ets:info(notification_user) of
	undefined ->
	    ets:new(notification_user, [set, public, named_table]);
	_Other4 ->
		ok
    end,
    process_flag(trap_exit, true),
    CheckErrorsPid = spawn_link(?MODULE, check_notification_errors, [self(), ServerAddress]),
    CheckOutdatedUsersPid = spawn_link(?MODULE, check_outdated_users, []),
    {ok, #notification_state{user_count = 0, 
			     check_errors_process_pid = CheckErrorsPid, 
			     check_outdated_users_process_pid = CheckOutdatedUsersPid,
			     address = ServerAddress}}.

handle_call({add_process, UserId, Key, ProcessPid, PacketId}, From, State) ->
    erlang:monitor(process, ProcessPid),
    spawn_link(?MODULE, add_process_internal, [From, UserId, Key, ProcessPid, PacketId]),
    {noreply, State};
handle_call({add_notification, ForUsers, FromUserId, Event}, From, State) ->
    spawn_link(?MODULE, add_notification_internal, [From, ForUsers, FromUserId, Event]),
    {noreply, State};
handle_call({add_channel_notification, ForUsers, FromUserId, Event}, From, State) ->
    spawn_link(?MODULE, add_channel_notification_internal, [From, ForUsers, FromUserId, Event]),
    {noreply, State};
handle_call(get_loading, _From, State) ->
    {reply, State#notification_state.user_count, State};
handle_call(Unknown, _From, State) ->
    {reply, Unknown, State}.

handle_cast({disconnect_user, UserId}, State) ->
    spawn_link(?MODULE, disconnect_user_internal, [UserId]),
    {noreply, State};
handle_cast({add_user, UserId, Key}, State) ->
    spawn_link(?MODULE, add_user_internal, [UserId, Key]),
    {noreply, State};
handle_cast({replace_user, UserId, Key}, State) ->
    spawn_link(?MODULE, replace_user_internal, [UserId, Key]),
    {noreply, State};
handle_cast({change_user_count, Increment}, State) ->
    {noreply, State#notification_state{user_count = State#notification_state.user_count + Increment}};
handle_cast(_Unknown, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    ?safe_try(error_logger:error_report([{type, notification_internal_process}, {reason, Reason}, {trace, erlang:get_stacktrace()}])),
    % restart special processes (process of checking errors or outdated users)
    if 
	State#notification_state.check_errors_process_pid == Pid ->
	    CheckErrorsPid = spawn_link(?MODULE, check_notification_errors, [self(), State#notification_state.address]),
	    {noreply, State#notification_state{check_errors_process_pid = CheckErrorsPid}};
	State#notification_state.check_outdated_users_process_pid == Pid ->
	    CheckOutdatedUsersPid = spawn_link(?MODULE, check_outdated_users, []),
	    {noreply, State#notification_state{check_outdated_users_process_pid = CheckOutdatedUsersPid}};
	true ->
	    {noreply, State}
    end;
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    delete_process(Pid),
    {noreply, State};
handle_info(_Message, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    stop(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal processes

add_channel_notification_internal(From, [], _FromUserId, _Event) ->
    gen_server:reply(From, ok);
add_channel_notification_internal(From, [ForUserId | RestUsers], FromUserId, Event) ->
    MessageId = ?datetime:now(),
    ets:insert(notification_queue, {ForUserId, FromUserId, MessageId, Event}),
    add_channel_notification_internal(From, RestUsers, FromUserId, Event).

add_notification_internal(From, [], _FromUserId, _Event) ->
    gen_server:reply(From, ok);
add_notification_internal(From, [ForUserId | RestUsers], FromUserId, Event) ->
    MessageId = ?datetime:now(),
    ets:insert(notification_queue, {ForUserId, FromUserId, MessageId, Event}),
    % search a waiting process and send notification to it if it exists
    case ets:lookup(notification_process, ForUserId) of
	[] ->
	    ok;
	[{UserId, PID, _CreatedDate}] ->
	    update_user_internal(UserId),
	    PID ! ?create_packet
    end,
    add_notification_internal(From, RestUsers, FromUserId, Event).

add_process_internal(From, UserId, Key, ProcessPid, OldPacketId) ->
    % delete previous notification's packet that was sent to client
    DeleteResult = if 
		       OldPacketId =/= undefined andalso OldPacketId > 0 ->
			   delete_packet(UserId, OldPacketId);
		       true ->
			   ok
		   end,
    % refresh last modified date in notification_user's table
    add_user_internal(UserId, Key),
    % check user's queue for new messages and send to the client process a message if it is not empty
    case ets:member(notification_queue, UserId) of
	false ->
	    add_process(UserId, ProcessPid);
	true ->
	    case DeleteResult of
		ok ->
		    ProcessPid ! ?create_packet;
		?retry_packet ->
		    ProcessPid ! ?retry_packet
	    end
    end,
    gen_server:reply(From, ok).

create_packet(UserId) ->
    case ets:lookup(notification_queue, UserId) of
	[] ->
	    #notification_result{};
	Queue ->
	    PacketId = ?datetime:now(),
	    {Notifications, MessageIds} = lists:mapfoldl(fun({_ForUserId, EventUserId, Id, Event}, Ids) -> 
							       {#notification{user_id = EventUserId, event = Event}, [Id | Ids]} 
						       end, [], Queue),
	    ets:insert(notification_packet, {UserId, PacketId, MessageIds}),
	    #notification_result{packet_id = PacketId,
			 notifications = Notifications}
    end.

retry_packet(UserId) ->
    case ets:lookup(notification_packet, UserId) of
	[] ->
	    #notification_result{};
	[{UserId, NewPacketId, MessageIds}] ->
	    case ets:lookup(notification_queue, UserId) of
		[] ->
		    #notification_result{};
		Queue ->
		    Notifications = lists:foldl(fun({_ForUserId, EventUserId, MessageId, Event}, Notifications) -> 
							case lists:member(MessageId, MessageIds) of
							    true ->
								[#notification{user_id = EventUserId, event = Event} | Notifications];
							    false ->
								Notifications
							end
						end, [], Queue),
		    #notification_result{packet_id = NewPacketId, notifications = Notifications}
	    end
    end.

add_user_internal(UserId, Key) ->
    case ets:member(notification_user, UserId) of
	true ->
	    ok;
	false ->
	    gen_server:cast(?MODULE, {change_user_count, 1})
    end,
    ets:insert( notification_user, {UserId, ?datetime:now(), Key} ).

replace_user_internal(UserId, Key) ->
    case ets:lookup(notification_process, UserId) of
	[] ->
	    ok;
	[{_UserId, PID, _CreatedDate}] ->
	    PID ! ?disconnect
    end,
    delete_notifications_by_user(UserId),
    case ets:member(notification_user, UserId) of
	true ->
	    ok;
	false ->
	    gen_server:cast(?MODULE, {change_user_count, 1})
    end,
    ets:insert( notification_user, {UserId, ?datetime:now(), Key} ).

update_user_internal(UserId) ->
    % update last modified date
    ets:update_element( notification_user, UserId, {2, ?datetime:now()} ).
  
disconnect_user_internal(UserId) ->
    case ets:lookup(notification_process, UserId) of
	[] ->
	    ok;
	[{_UserId, PID, _CreatedDate}] ->
	    PID ! ?disconnect
    end,
    delete_user(UserId),
    delete_notifications_by_user(UserId).

check_notification_errors(ServerPid, ServerAddress) ->
    timer:sleep(?datetime:to_milliseconds(?CHECK_NOTIFICATION_ERROR_INTERVAL)),
    %% Update last modified date of notification server
    moodbox_database:add_notification_server(ServerPid, ServerAddress),
    F = fun() -> mnesia:index_read(notification_error, ServerPid, #notification_error.server_pid) end,
    NotificationErrors = mnesia:activity(transaction, F, [], mnesia_frag),
    handle_notification_errors(NotificationErrors),
    check_notification_errors(ServerPid, ServerAddress).

check_outdated_users() ->
    timer:sleep(?datetime:to_milliseconds(?CHECK_NOTIFICATION_USER_INTERVAL)),
    MinimumDateTime = ?datetime:add(?datetime:now(), -?NOTIFICATION_USER_TIMEOUT), 
    % $1 - user_id, $2 - last_modified_date, $3 - key
    OutdatedUsers = ets:select(notification_user, [{{'$1', '$2', '$3'},[{'<', '$2', MinimumDateTime}], [['$1', '$3']]}]),
    handle_outdated_users(OutdatedUsers),
    check_outdated_users().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions

handle_outdated_users([]) ->
    ok;
handle_outdated_users([ [UserId, Key] | Rest]) ->
    case ets:lookup(notification_process, UserId) of
	[] ->
	    moodbox_facade:notification_unregister(#context{user_id = UserId}, Key, true);
	_Other ->
	    ok
    end,
    handle_outdated_users(Rest).

handle_notification_errors([]) ->
    ok;
handle_notification_errors([Error | RestErrors]) ->
    delete_notifications_by_user(Error#notification_error.user_id),
    case ets:lookup(notification_process, Error#notification_error.user_id) of
	[] ->
	    MessageId = ?datetime:now(),
	    ets:insert(notification_queue, {Error#notification_error.user_id, undefined, MessageId, ?reload, undefined});
	[{_UserId, PID, _CreatedDate}] ->
	    PID ! {self(), #notification_result{notifications = [#notification{event = ?reload}]}}
    end,
    F = fun() -> mnesia:delete({notification_error, Error#notification_error.user_id}) end,
    mnesia:activity(transaction, F, [], mnesia_frag),
    handle_notification_errors(RestErrors).

delete_user(UserId) ->
    case ets:member(notification_user, UserId) of
	true ->
	    ets:delete(notification_user, UserId),
            gen_server:cast(?MODULE, {change_user_count, -1});
	false ->
	    ok
    end.

add_process(UserId, ProcessPid) ->
    ets:insert(notification_process, {UserId, ProcessPid, ?datetime:now()}).

delete_process(UserId) when is_integer(UserId) ->
    ets:delete(notification_process, UserId);
delete_process(Pid) when is_pid(Pid) ->
    ets:match_delete(notification_process, {'_', Pid, '_'}).

delete_packet(UserId, PacketId) ->
   case ets:lookup(notification_packet, UserId) of
       [{UserId, PacketId, MessageIds}] ->
	   delete_notifications(UserId, MessageIds),
	   ets:match_delete(notification_packet, {'_', PacketId, '_'}),
	   ok;
       [] ->
	   ok;
       Other ->
	   ?safe_try(error_logger:error_report([{type, delete_notification_packet}, {user_id, UserId}, {packet_id, PacketId}, {value, Other}])),
	   ?retry_packet
   end.

delete_notifications(_UserId, []) ->
    ok;
delete_notifications(UserId, [Id | Rest]) ->
    ets:match_delete(notification_queue, {UserId, '_', Id, '_'}),
    delete_notifications(UserId, Rest).

delete_notifications_by_user(UserId) ->
    ets:delete(notification_queue, UserId),
    ets:delete(notification_packet, UserId).

get_notification_server_address() ->
    case moodbox_parameter:read_parameter(?ParameterFile, "MoodBox_OverrideNotificationServerAddress") of
	undefined ->
	    moodbox_parameter:read_parameter(?ParameterFile, "MoodBox_InitialPublicServerAddress");
	Value ->
	    Value
    end.
