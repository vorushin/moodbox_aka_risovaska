%%%-------------------------------------------------------------------
%%% @doc    Main moodbox application module.
%%% @since  Mon 16 SEP 2008 02:08:16 PM MSK
%%% @end -------------------------------------------------------------

-module(moodbox).
-behaviour(application).

%% External exports
-export([start_script/0, start_script/1, stop_script/1]).

%% application callbacks exports
-export([start/0,
	 start/2,
	 stop/0,
	 stop/1
        ]).

-include("moodbox.hrl").
-include("moodbox_config.hrl").
-include("moodbox_database.hrl").
-include("moodbox_repository.hrl").

-define(parameter, moodbox_parameter).

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
%% @doc 
%% @spec start_script() -> ok
start_script() ->
    start().

start_script([init]) ->
    init:stop();
start_script([init_test]) ->
    init:stop();
start_script([init_ssl]) ->
    application:load(?APP),
    init:stop().

stop_script([Node]) ->
    case net_adm:ping(Node) of
	pong ->
            io:format("Stop:~p~n",[Node]),
            rpc:cast(Node, init, stop, []);
	pang ->
	    io:format("There is no node with name ~p",[Node])
    end,
    init:stop().

%%%-------------------------------------------------------------------
%%% Callback functions for application
%%%-------------------------------------------------------------------
start() ->
    application:start(?APP).

start(_Type, _Args) ->
    start_error_logger(),
    sasl_start(),
    module_start(inets),
    module_start(ssl),
    module_start(crypto),
    ssl:seed(crypto:rand_bytes(256)),
    % start 'starling' library under Unix
    case os:type() of
	{win32, nt} ->
	    ok;
	_OtherType ->
	    application:start(starling_app)
    end,
    Mode = ?parameter:read_parameter(?ParameterFile, "MoodBox_Mode"),
    ServiceAddress = ?parameter:read_parameter(?ParameterFile, "MoodBox_ServiceAddress"),
    SchemaNode = ?parameter:read_parameter(?ParameterFile, "MoodBox_SchemaNode"),
    case Mode of
	?NOTIFICATION_SERVER ->
	    mnesia:start(),
	    Node = get_schema_node_name(SchemaNode, ServiceAddress),
	    include_node_in_schema(Node, false, true, [{?NOTIFICATION_SERVER_TABLE_LIST, ram_copies}]),
	    mnesia:stop();
	?SERVER ->
	    mnesia:start(),
	    Node = get_schema_node_name(SchemaNode, ServiceAddress),
	    include_node_in_schema(Node, true, false, [{?SERVER_TABLE_LIST, disc_copies}, {?NOTIFICATION_SERVER_TABLE_LIST, ram_copies}]),
	    mnesia:stop();
	?DB_SERVER ->
	    case get_schema_node_name(SchemaNode, ServiceAddress) of
		undefined ->
		    moodbox_database:create();
		Node ->
		    mnesia:start(),
		    include_node_in_schema(Node, true, false, [{?SERVER_TABLE_LIST, disc_copies}, {?NOTIFICATION_SERVER_TABLE_LIST, ram_copies}])
	    end,
	    mnesia:stop();
	_Other ->
	    ok
    end,
    moodbox_sup:start_link(Mode).

stop() ->
    application:stop(?APP),
    inets:stop(),
    ssl:stop(),
    crypto:stop(),
    application:stop(sasl).

stop(_State) ->
    ok.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

sasl_start() ->
    case catch application:start(sasl) of
        ok ->
            ok;
        {error,{already_started,sasl}} ->
            ok;
        Err ->
            error_logger:format("Failed to start sasl: ~p~n", [Err])
    end.

module_start(Module) ->
    case catch Module:start() of
        ok ->
            ok;
        {error, {already_started, Module}} ->
            ok;
        Err ->
            error_logger:format("Failed to start ~p: ~p~n", [Module, Err])
    end.

start_error_logger() ->
    logger:init([]).

include_node_in_schema(Node, CheckExistedDB, IsDiscLessNode, TableListWithCopyType) ->
    if 
	CheckExistedDB ->
	    case mnesia:system_info(tables) of
		[schema] -> % start the first time
		    IsDBExists = false;
		_Other ->
		    IsDBExists = true
	    end;
	true ->
	    IsDBExists = false
    end,
    if 
	IsDBExists == false ->
	    case rpc:call(list_to_atom(Node), mnesia, change_config, [extra_db_nodes, [node()]]) of
		{ok, [_MyNode]} ->
		    if 
			IsDiscLessNode == false ->
			    mnesia:change_table_copy_type(schema, node(), disc_copies);
			true ->
			    ok
		    end,
		    replicate_tables(TableListWithCopyType);
		{ok, []} ->
		    error_logger:error_report([{type, add_node_to_schema}, {exception, "Can't add the node to schema"},
					       {trace, erlang:get_stacktrace()}]);
		{error, Reason} ->
		    error_logger:error_report([{type, add_node_to_schema}, {exception, Reason}, {trace, erlang:get_stacktrace()}]),
		    exit(Reason)
	    end;
	true ->
	    ok
   end.

replicate_tables([]) ->
    ok;
replicate_tables([{TableList, CopyType} | Rest]) ->
    replicate_table(TableList, CopyType),
    replicate_tables(Rest).

replicate_table([], _CopyType) ->
    ok;
replicate_table([Table | Rest], CopyType) ->
    mnesia:add_table_copy(Table, node(), CopyType),
    replicate_table(Rest, CopyType).

get_schema_node_name(SchemaNode, ServiceAddress) ->
    if 
	SchemaNode == undefined andalso ServiceAddress == undefined ->
	    undefined;
	SchemaNode == undefined ->
	    {ok, Node} = moodbox_request:server_call_std_model(lists:flatten(io_lib:format(?SERVER_URL, [ServiceAddress])), get_srv_address, []),
	    Node;
	true ->
	    SchemaNode
    end.
