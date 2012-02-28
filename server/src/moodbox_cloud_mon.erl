-module(moodbox_cloud_mon).

-export([show/0, show/1, rebalance/0, rebalance/1]).
-export([shuffle/1]).

-define(Ec2Key, {"your_key", "your_sec_key"}).
-define(GroupsToSkip, [g_web, g_web_test]).

-include("moodbox_ec2.hrl").

-define(ec2, moodbox_ec2).
-define(request, moodbox_request).
-define(balancer, moodbox_nginx).

-record(server_info, {group, instance_id, state, types, balancing_state, instance_info}).

%%% @todo db schema monitoring, notification servers registration monitoring, does load balancers have elastic ips

get_state_order(running) -> 0;
get_state_order(not_responding) -> 10;
get_state_order({not_runnging, _}) -> 20;
get_state_order(undefined) -> 100000.

get_balancing_state_order(ok) -> 0;
get_balancing_state_order(has_extra) -> 10;
get_balancing_state_order(some_missed) -> 20;
get_balancing_state_order(unbalanced) -> 30;
get_balancing_state_order(undefined) -> 100000.

get_server_balancing_state_order(ok) -> 0;
get_server_balancing_state_order(some_balancers) -> 10;
get_server_balancing_state_order(no_balancers) -> 20;
get_server_balancing_state_order(unbalanced) -> 30;
get_server_balancing_state_order(undefined) -> 100000.

show() ->
    show(undefined).
show(ForGroup) ->
    Instances = get_servers_info(ForGroup, ?ec2:describe_instances(?Ec2Key)),
    SortedInstances = lists:keysort(#server_info.group, Instances),
    show_groups(SortedInstances).

show_groups([] = _Instances) ->
    ok;
show_groups([#server_info{group = Group} = _Instance | _Tail] = Instances) ->
    {GroupInstances, Rest} = lists:splitwith(fun(#server_info{group = CurrentGroup}) -> if CurrentGroup == Group -> true; true -> false end end, Instances),
    show_group(Group, GroupInstances),
    show_groups(Rest).

show_group(Group, Instances) ->
    io:format("~nGroup: ~s~n", [Group]),
    Balancers = lists:filter(fun is_balancer/1, Instances),
    Servers = lists:filter(fun is_app_server/1, Instances),
    case Balancers of
	[] ->
	    io:format("  LoadBalancers:        Not Found~n", []);
	_ ->
	    io:format("  LoadBalancers:~n", []),
	    SortedServerAddresses = lists:usort(extract_private_addresses(Servers)),
	    BalancersInfo = lists:map(fun(Balancer) -> get_balancer_info(Balancer, SortedServerAddresses) end, Balancers),
	    SortedBalancersInfo = lists:sort(fun sort_balancers/2, BalancersInfo),
	    lists:foreach(fun show_balancer/1, SortedBalancersInfo)
    end,
    case Servers of
	[] ->
	    io:format("  Servers:              Not Found~n", []);
	_ ->
	    io:format("  Servers:~n", []),
	    SortedServers = lists:sort(fun sort_servers/2, Servers),
	    lists:foreach(fun show_server/1, SortedServers)
    end,
    case lists:filter(fun is_notification_server/1, Instances) of
	[] ->
	    io:format("  NotificationServers:  Not Found~n", []);
	NotificationServers ->
	    io:format("  NotificationServers:~n", []),
	    SortedNotificationServers = lists:sort(fun sort_notification_servers/2, NotificationServers),
	    lists:foreach(fun show_notification_server/1, SortedNotificationServers)
    end,
    case lists:filter(fun is_db_server/1, Instances) of
	[] ->
	    io:format("  DbServers:            Not Found~n", []);
	DbServers ->
	    io:format("  DbServers:~n", []),
	    SortedDbServers = lists:sort(fun sort_db_servers/2, DbServers),
	    lists:foreach(fun show_db_server/1, SortedDbServers)
    end,
    case lists:filter(fun is_unknown_server/1, Instances) of
	[] ->
	    ok;
	UnknownServers ->
	    io:format("  UnknownServers:~n", []),
	    SortedUnknownServers = lists:sort(fun sort_unknown_servers/2, UnknownServers),
	    lists:foreach(fun show_unknown_server/1, SortedUnknownServers)
    end.


show_balancer(#server_info{instance_id = InstanceId, state = State, balancing_state = BalancingState, types = Types}) ->
    io:format("    ~-17s - ", [InstanceId]),
    case State of
	{StateItself, AdditionalInfo} ->
	    io:format("~s (~s)", [StateItself, AdditionalInfo]);
	_ ->
	    io:format("~s", [State])
    end,
    io:format(" ~s", [BalancingState]),
    case lists:delete(load_balancer, Types) of
	[] ->
	    io:format("~n", []);
	ExtraTypes ->
	    io:format(" ~w~n", [ExtraTypes])
    end.
sort_balancers(#server_info{instance_id = InstanceIdA, state = StateA, balancing_state = BalancingStateA}, #server_info{instance_id = InstanceIdB, state = StateB, balancing_state = BalancingStateB}) ->
    StateOrderA = get_state_order(StateA),
    StateOrderB = get_state_order(StateB),
    if
	StateOrderA == StateOrderB ->
	    BalancingStateOrderA = get_balancing_state_order(BalancingStateA),
	    BalancingStateOrderB = get_balancing_state_order(BalancingStateB),
	    if
		BalancingStateOrderA == BalancingStateOrderB ->
		    InstanceIdA < InstanceIdB;
		BalancingStateOrderA < BalancingStateOrderB ->
		    true;
		true ->
		    false
	    end;
	StateOrderA < StateOrderB ->
	    true;
	true ->
	    false
    end.

show_server(#server_info{instance_id = InstanceId, state = State, balancing_state = BalancingState, types = Types}) ->
    io:format("    ~-17s - ", [InstanceId]),
    case State of
	{StateItself, AdditionalInfo} ->
	    io:format("~s (~s)", [StateItself, AdditionalInfo]);
	_ ->
	    io:format("~s", [State])
    end,
    io:format(" ~s", [BalancingState]),
    case lists:delete(server, Types) of
	[] ->
	    io:format("~n", []);
	ExtraTypes ->
	    io:format(" ~w~n", [ExtraTypes])
    end.
sort_servers(#server_info{instance_id = InstanceIdA, state = StateA, balancing_state = BalancingStateA}, #server_info{instance_id = InstanceIdB, state = StateB, balancing_state = BalancingStateB}) ->
    StateOrderA = get_state_order(StateA),
    StateOrderB = get_state_order(StateB),
    if
	StateOrderA == StateOrderB ->
	    BalancingStateOrderA = get_server_balancing_state_order(BalancingStateA),
	    BalancingStateOrderB = get_server_balancing_state_order(BalancingStateB),
	    if
		BalancingStateOrderA == BalancingStateOrderB ->
		    InstanceIdA < InstanceIdB;
		BalancingStateOrderA < BalancingStateOrderB ->
		    true;
		true ->
		    false
	    end;
	StateOrderA < StateOrderB ->
	    true;
	true ->
	    false
    end.

simple_show_server(Type, #server_info{instance_id = InstanceId, state = State, types = Types}) ->
    io:format("    ~-17s - ", [InstanceId]),
    case State of
	{StateItself, AdditionalInfo} ->
	    io:format("~s (~s)", [StateItself, AdditionalInfo]);
	_ ->
	    io:format("~s", [State])
    end,
    case lists:delete(Type, Types) of
	[] ->
	    io:format("~n", []);
	ExtraTypes ->
	    io:format(" ~w~n", [ExtraTypes])
    end.
simple_sort_servers(#server_info{instance_id = InstanceIdA, state = StateA}, #server_info{instance_id = InstanceIdB, state = StateB}) ->
    StateOrderA = get_state_order(StateA),
    StateOrderB = get_state_order(StateB),
    if
	StateOrderA == StateOrderB ->
	    InstanceIdA < InstanceIdB;
	StateOrderA < StateOrderB ->
	    true;
	true ->
	    false
    end.

show_notification_server(Server) ->
    simple_show_server(notification_server, Server).
sort_notification_servers(ServerA, ServerB) ->
    simple_sort_servers(ServerA, ServerB).

show_db_server(Server) ->
    simple_show_server(db_server, Server).
sort_db_servers(ServerA, ServerB) ->
    simple_sort_servers(ServerA, ServerB).

show_unknown_server(Server) ->
    simple_show_server(undefined, Server).
sort_unknown_servers(ServerA, ServerB) ->
    simple_sort_servers(ServerA, ServerB).



rebalance() ->
    rebalance(undefined).
rebalance(ForGroup) ->
    Instances = get_servers_info(ForGroup, ?ec2:describe_instances(?Ec2Key)),
    LiveList = lists:filter(fun(Instance) -> case Instance of #server_info{state = running} -> true; true -> false end end, Instances),
    Balancers = lists:filter(fun is_balancer/1, LiveList),
    case Balancers of
	[] ->
	    io:format("Nothing to rebalance for group ~s!~n", [ForGroup]);
	_ ->
	    Servers = lists:filter(fun is_app_server/1, LiveList),
	    assign_servers_for_balancers(Balancers, Servers),
	    io:format("~ndone!~n~n", [])
    end.

assign_servers_for_balancers([] = _Balancers, _Servers) ->
    ok;
assign_servers_for_balancers([Balancer | Tail] = _Balancers, Servers) ->
    io:format("Balancing ~s", [Balancer#server_info.instance_id]),
    io:format(" - ~s~n", [assign_servers_for_balancer(Balancer, Servers)]),
    assign_servers_for_balancers(Tail, Servers).

assign_servers_for_balancer(Balancer, Servers) ->
    Config = get_balancer_config(Balancer),
    BalancedAddresses = lists:usort(?balancer:get_balanced_addresses(Config)),
    ServerAddresses = lists:usort(extract_private_addresses(Servers)),
    if
	BalancedAddresses == ServerAddresses ->
	    already_ok;
	true ->
	    NewAddresses = shuffle(ServerAddresses),
	    NewConfig = ?balancer:create_config(NewAddresses),
	    ok = update_balancer_config(Balancer, NewConfig),
	    ok
    end.


get_servers_info(ForGroup, Reservations) ->
    get_servers_info(ForGroup, Reservations, []).
get_servers_info(_ForGroup, [] = _Reservations, Result) ->
    lists:reverse(Result);
get_servers_info(ForGroup, [Reservation | Tail], Result) ->
    NewResult = get_servers_info_for_reservation(ForGroup, Reservation, Result),
    get_servers_info(ForGroup, Tail, NewResult).
get_servers_info_for_reservation(ForGroup, #ec2_reservation_info{groups = Groups, instances = Instances} = _Reservation, Result) ->
    Types = get_types(Groups),
    Group = get_group(Groups),
    case ForGroup == Group orelse ForGroup =:= undefined andalso not(lists:member(Group, ?GroupsToSkip)) of
	true ->
	    get_servers_info_for_instances(Group, Types, Instances, Result);
	false ->
	    Result
    end.
get_servers_info_for_instances(_Group, _Types, [] = _Instances, Result) ->
    Result;
get_servers_info_for_instances(Group, Types, [#ec2_instance{instance_id = InstanceId} = Instance | Tail] = _Instances, Result) ->
    get_servers_info_for_instances(Group, Types, Tail, [#server_info{group = Group, instance_id = InstanceId, types = Types, instance_info = Instance, state = get_state(Instance)} | Result]).


get_balancer_info(Balancer, SortedServerAddresses) ->
    Config = get_balancer_config(Balancer), % @todo state = not_responding
    BalancedAddresses = lists:usort(?balancer:get_balanced_addresses(Config)),
    if
	BalancedAddresses == SortedServerAddresses ->
	    Balancer#server_info{balancing_state = ok};
	true ->
	    Balancer#server_info{balancing_state = unbalanced} % @todo state = have_extra | missed_some | unbalanced
    end.

get_group([] = _Groups) ->
    undefined;
get_group([#ec2_group{group_id = Group} | Tail] = _Groups) ->
    case atom_to_list(Group) of
	[$g, $_ | _] ->
	    Group;
	_ ->
	    get_group(Tail)
    end.

get_types(Groups) ->
    get_types(Groups, []).
get_types([] = _Groups, Result) ->
    lists:reverse(Result);
get_types([#ec2_group{group_id = Group} | Tail] = _Groups, Result) ->
    case lists:member(Group, [load_balancer, server, notification_server, db_server]) of
	true ->
	    get_types(Tail, [Group | Result]);
	false ->
	    get_types(Tail, Result)
    end.

get_state(#ec2_instance{instance_state = #ec2_instance_state{name = State}} = _Instance) ->
    case State of
	undefined ->
	    State;
	running ->
	    running;
	_ ->
	    {not_running, State}
    end.

is_balancer(#server_info{types = Types}) ->
    lists:member(load_balancer, Types);
is_balancer(_) ->
    false.

is_app_server(#server_info{types = Types}) ->
    lists:member(server, Types);
is_app_server(_) ->
    false.

is_notification_server(#server_info{types = Types}) ->
    lists:member(notification_server, Types);
is_notification_server(_) ->
    false.

is_db_server(#server_info{types = Types}) ->
    lists:member(db_server, Types);
is_db_server(_) ->
    false.

is_unknown_server(Server) ->
    not(is_balancer(Server) orelse is_app_server(Server) orelse is_notification_server(Server) orelse is_db_server(Server)).


shuffle([] = List) ->
    List;
shuffle([_Item] = List) ->
    List;
shuffle(List) ->
    Len = 1000 * length(List),
    lists:map(fun({_Rand, Value} = _Item) -> Value end, lists:keysort(1, lists:map(fun(Item) -> {crypto:rand_uniform(1, Len), Item} end, List))).

extract_private_addresses(ServerInfoItems) ->
    lists:map(fun(Item) -> (Item#server_info.instance_info)#ec2_instance.private_dns_name end, ServerInfoItems).

get_balancer_config(Balancer) ->
    {ok, Result} = ?request:server_erl_call(io_lib:format("https://~s/serve", [(Balancer#server_info.instance_info)#ec2_instance.dns_name]), get_config, []),
    Result.
update_balancer_config(Balancer, NewConfig) ->
    {ok, Result} = ?request:server_erl_call(io_lib:format("https://~s/serve", [(Balancer#server_info.instance_info)#ec2_instance.dns_name]), update_config, [NewConfig]),
    Result.
