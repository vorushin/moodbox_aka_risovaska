-module(moodbox_server).

-behavior(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([start/0, stop/1]).

-include_lib("mochiweb/include/mochiweb.hrl").

-include("moodbox_config.hrl").
-include("moodbox_facade.hrl").
-include("moodbox_database.hrl").
-include("moodbox_context.hrl").
-include("moodbox_auth.hrl").

-define(WebRootPath, "priv/web_root").
-define(TestDirPath, "priv/test").

-define(parameter, moodbox_parameter).

-define(ctx, moodbox_context).
-define(facade, moodbox_facade).

-define(model_definition, moodbox_model_definition).
-define(model, moodbox_model).
-define(parser, moodbox_xml_parser).
-define(generator, moodbox_xml_generator).
-define(json_generator, moodbox_json_generator).

-define(InternalErrorCode, "InternalError").
-define(InternalErrorMessage, "Internal server error").

-define(safe_try(Expr), try Expr catch _:_ -> undefined end).

%%% exceptions
-export([get_exception_message/1]).
-include("moodbox_exception.hrl").
-record(unsupported_client_version_exception, {context}).
-record(parse_exception, {data}).
-record(network_exception, {operation, data}).
-record(extended_exception, {data}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start(?MODULE, [], []).

stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop).

%%%-------------------------------------------------------------------
%%% Callbacks functions for gen_server
%%%-------------------------------------------------------------------
init([]) ->
    mnesia:wait_for_tables(?SERVER_TABLE_LIST, 60000),

    Model = ?model:create(?model_definition:get_definition()),
    
    % define request handler
    Handler = fun(Req) -> handle(Req, Model) end,

    % start mochiweb
    Ssl = #ssl{keyfile = ?SSL_KEYFILE, certfile = ?SSL_CERTFILE, verify = ?SSL_VERIFY, password = ?SSL_PASSWORD, cacertfile = ?SSL_CACERTFILE, 
	       ciphers = ?SSL_CIPHERS},
    case is_port_enabled("MoodBox_IsHttpsEnabled") of
	true ->
	    mochiweb_http:start([{loop, Handler}, {name, moodbox_https}, {port, get_port("MoodBox_HttpsPort", 443)}, {ssl, Ssl}]);
	false ->
	    ok
    end,
    case is_port_enabled("MoodBox_IsHttpEnabled") of
	true ->
	    mochiweb_http:start([{loop, Handler}, {name, moodbox_http}, {port, get_port("MoodBox_HttpPort", 80)}]);
	false ->
	    ok
    end,
    
    % @todo investigate the problem with mochiweb server
    %process_flag(trap_exit, true),
    {ok, []}.

is_port_enabled(Name) ->
    case ?parameter:read_parameter(?ParameterFile, Name) of
	undefined ->
	    true;
	Value ->
	    case list_to_atom(Value) of
		true ->
		    true;
		_ ->
		    false
	    end
    end.
get_port(Name, Default) ->
    case ?parameter:read_parameter(?ParameterFile, Name) of
	undefined ->
	    Default;
	Port ->
	    list_to_integer(Port)
    end.

handle_call({command, Message}, _From, State) ->
    {noreply, Message, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Port, Reason}, State) ->
    {stop, {port_terminated, Reason}, State};

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate({port_terminated, _Reason}, _State) ->
    ok;
terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(Req, Model) ->
    [$/ | Path] = Req:get(path),
    % io:format("Path: ~p~n", [Path]),
    case Path of
	"do" ->
	    handle_do(Req, Model);
	"json" ->
	    handle_do(Req, Model, json);
	"server_statistics" ->
	    Req:ok({"text/html", moodbox_statistics:get_statistics()});
	"multi" ->
	    handle_multi(Req, Model);
	_ ->
	    Req:serve_file(Path, ?WebRootPath)
    end.


handle_do(Req, Model) ->
    handle_do(Req, Model, xml).
handle_do(Req, Model, Proto) ->
    try
        % @todo ensure it's a POST with XML inside sent to valid URL

        % obtain request raw data
	RawData = recv(Req),
	case RawData of
	    undefined -> % got it on GET requests, need special handling or else there will be very verbose stacktrace
		?error(#parse_exception{data = RawData});
	    <<>> -> % not necessary, but why not if we anyway must do check for 'undefined'
		?error(#parse_exception{data = RawData});
	    _ ->
                %file:write_file(filename:join(?TestDirPath, "last_request.xml"), RawData),
                %io:format("RawData: ~w~n", [RawData]),

		{Context, Func, Args} = parse_request(RawData, Model),

		try
		    check_context(Model, Context, Func),

		    Result = apply(?facade, Func, [Context | Args]),
                    %io:format("Result: ~w~n", [Result]),

		    Response = prepare_response(Model, Func, Result, Proto),
		    respond(Req, Response)
		catch
		    ExceptionTypeToExtend:ExceptionToExtend ->
			raise_extended(ExceptionTypeToExtend, ExceptionToExtend, {Context, Func, Args})
		end
	end
    catch
	ExceptionType:Exception ->
	    handle_exception(Req, Model, ExceptionType, Exception, Proto)
    end.

handle_exception(Req, Model, ExceptionType, Exception, Proto) ->
    Fault = create_fault(Req, ExceptionType, Exception), % MUST NOT throw any exceptions
    Result = 
	try
	    case get_exception_reaction(ExceptionType, Exception) of
		{exit, _Mode} = Exit ->
		    Exit; % exit will also close keep-alive connection (socket)
		normal ->
    		    %file:write_file(filename:join(?TestDirPath, "last_fault.txt"), io_lib:format("~p", [Fault])),
		    Response = prepare_response(Model, Fault, Proto),
	            %file:write_file(filename:join(?TestDirPath, "last_fault.xml"), Response),
		    respond(Req, Response),
		    ok
	    end
	catch
	    NewExceptionType:NewException ->
		Trace = ?safe_try(erlang:get_stacktrace()),
		?safe_try(error_logger:error_report([{type, NewExceptionType}, {error, NewException}, {where, handle_exception}, {fault, Fault}, {trace, Trace}])),
		{exit, normal}
	end,
    case Result of
	{exit, Mode} ->
	    Socket = Req:get(socket), % close socket before exit
	    case Req:get(ssl) of
		true ->
		    ssl:close(Socket);
		_ ->
		    gen_tcp:close(Socket)
	    end,
	    exit(Mode);
	_ ->
	    ok
    end.

parse_request(RawData, Model) ->
    try
        % parse and extract header and ActionData components
	#envelope{header = Header, body = ActionData} = ?parser:parse(Model, envelope, RawData),
        %io:format("HeaderData: ~w, ActionData: ~w~n", [Header, ActionData]),

	[Func | Args] = tuple_to_list(ActionData),
        %io:format("Action: ~w~n", [{moodbox_facade, Func, Args}]),

        % create context by header
	Context = ?ctx:create_context(Header),
        %io:format("Context: ~w~n", [Context]),

	{Context, Func, Args}
    catch
	ExceptionType:Exception ->
	    ?error_inner(#parse_exception{data = RawData}, ExceptionType, Exception)
    end.    

prepare_response(Model, Func, Result, Proto) ->
    prepare_response(Model, {?model:get_result_type(Func), Result}, Proto).
prepare_response(Model, Data, Proto) ->
    % wrap data into envelope
    ResultEnvelope = #envelope{body = Data},
    %io:format("ResultEnvelope: ~w~n", [ResultEnvelope]),
                        
    % prepare XML or JSON
    case Proto of
	xml ->
	    ResponseData = ?generator:generate(Model, envelope, ResultEnvelope, fun collect/2, <<>>);
	json ->
	    ResponseData = ?json_generator:generate(Model, envelope, ResultEnvelope, fun collect/2, <<>>)
    end,
    %io:format("ResponseData: ~w~n", [ResponseData]),
    ResponseData.

recv(Req) ->
    try
	Req:recv_body(?MAX_REQUEST_SIZE)
    catch
	ExceptionType:Exception ->
	    ?error_inner(#network_exception{operation = recv}, ExceptionType, Exception)
    end.
respond(Req, Response) ->
    try
	Req:ok({"text/xml", Response})
    catch
	ExceptionType:Exception ->
	    ?error_inner(#network_exception{operation = respond, data = Response}, ExceptionType, Exception)
    end.

collect(Bin, Collected) ->
    %io:format("~s", [binary_to_list(Bin)]),
    << Collected/binary, Bin/binary >>.


handle_multi(Req, _Model) ->
    Callback = fun multi_callback/1,
    mochiweb_multipart:parse_multipart_request(Req, Callback),
    Req:ok({"text", <<"ok">>}).

multi_callback(Value) ->
    io:format("    ~p~n", [Value]),
    fun multi_callback/1.


%%% service functions                                   

check_context(Model, #context{user_id = UserId, client_version_tag = ClientVersionTag, client_version = ClientVersion} = Context, Function) ->
    case moodbox_version:check_version(ClientVersionTag, ClientVersion) of
	not_supported ->
	    ?error(#unsupported_client_version_exception{context = Context});
	supported ->
	    check_auth(Model, UserId, Function);
	undefined ->
	    check_auth(Model, UserId, Function)
    end.
check_auth(_Model, UserId, _Function) when UserId =/= undefined ->
    ok;
check_auth(Model, undefined, Function) ->
    case ?model:is_auth_free(Model, Function) of
	true ->
	    ok;
	false ->
	    ?error_ext(moodbox_auth, #bad_auth_exception{})
    end.
    

-ifdef(DEBUG).
-define(Details, get_fault_details(LogEntry)).
-else.
-define(Details, undefined).
-endif.

-define(iif(Value, ValueIfUndefined), if Value == undefined -> ValueIfUndefined; true -> Value end).

create_fault(Req, ExceptionType, Exception) ->
    try
	{FaultCode, Message, MustBeLogged} = case Exception of
						 #?exception{} ->
						     Code = ?exception:get_code(Exception),
						     Msg = ?exception:get_message(Exception),
						     {get_fault_code(Code),
						      ?iif(Msg, ?InternalErrorMessage),
						      must_be_logged(Code)};
						 _Else ->
						     {?InternalErrorCode,
						      ?InternalErrorMessage,
						      true}
					     end,
	Peer = get_peer_from_headers(Req),
	LogEntry = if
		       MustBeLogged ->
			   FlatMsg = case is_list(Message) of true -> lists:flatten(Message); _ -> Message end,
			   [{peer, Peer}, {code, FaultCode}, {description, FlatMsg}, {type, ExceptionType}, {error, Exception}, {trace, erlang:get_stacktrace()}]; % note if list structure is changed you need to update get_fault_details as well
		       true ->
			   undefined
		   end,
	Details = ?Details,
	if 
	    MustBeLogged ->
		error_logger:error_report(LogEntry);
	    true ->
		ok
	end,
	#fault{code = FaultCode, description = Message, details = Details}
    catch
	NewExceptionType:NewException ->
	    Trace = ?safe_try(erlang:get_stacktrace()),
	    ?safe_try(error_logger:error_report([{type, NewExceptionType}, {error, NewException}, {where, {create_fault, [ExceptionType, Exception]}}, {trace, Trace}])),
	    #fault{code = ?InternalErrorCode, description = ?InternalErrorMessage}
    end.

get_peer_from_headers(undefined) ->
    undefined;
get_peer_from_headers(Req) ->
    case Req:get_header_value("x-forwarded-for") of
	undefined ->
	    undefined;
	Hosts ->
	    string:strip(lists:last(string:tokens(Hosts, ",")))
    end.

get_fault_details(undefined) ->
    undefined;
get_fault_details([Peer, _Code, _Message | Details] = _LogEntry) ->
    io_lib:format("~p", [[Peer | Details]]).

get_fault_code({?MODULE, parse_exception}) ->
    "BadRequest";
get_fault_code({?MODULE, unsupported_client_version_exception}) ->
    "UnsupportedClientVersion";
get_fault_code({moodbox_auth, bad_auth_exception}) ->
    "NotAuthenticated";
get_fault_code({moodbox_facade, notification_session_not_found}) ->
    "NotificationSessionNotFound";
get_fault_code({_HandlingModule, _ExceptionType}) ->
    ?InternalErrorCode.

must_be_logged({?MODULE, parse_exception}) ->
    true;
must_be_logged({?MODULE, unsupported_client_version_exception}) ->
    false;
must_be_logged({?MODULE, network_exception}) ->
    true;
must_be_logged({moodbox_auth, bad_auth_exception}) ->
    true;
must_be_logged({moodbox_facade, notification_session_not_found}) ->
    false;
must_be_logged(_Exception) ->
    true.


raise_extended(ExceptionType, #?exception{additional_data = OldData} = Exception, Data) ->
    case must_be_logged(Exception) of
	true ->
	    erlang:raise(ExceptionType, Exception#?exception{additional_data = combine_additional_data(Data, OldData)}, erlang:get_stacktrace());
	false ->
	    erlang:raise(ExceptionType, Exception, erlang:get_stacktrace())
    end;
raise_extended(ExceptionType, Exception, Data) ->
    ?error_inner(#extended_exception{data = Data}, ExceptionType, Exception).

combine_additional_data(Data, undefined = _OldData) ->
    [Data];
combine_additional_data(Data, OldData) when is_list(OldData) ->
    [Data | OldData].

get_exception_reaction(exit, Exception) ->
    {exit, Exception};
get_exception_reaction(_ExceptionType, ?exception_data(#network_exception{}) = _Exception) ->
    {exit, normal};
get_exception_reaction(_ExceptionType, #?exception{inner_exception_type = InnerExceptionType, inner_exception = InnerException} = _Exception) ->
    get_exception_reaction(InnerExceptionType, InnerException);
get_exception_reaction(_ExceptionType, _Exception) ->
    normal.


%%% exceptions handling

get_exception_message(?exception_data(#unsupported_client_version_exception{})) ->
    "This client or API version is no longer suported";
get_exception_message(?exception_data(#parse_exception{}) = Exception) ->
    #?exception{inner_exception = InnerException} = Exception,
    case InnerException of
	#?exception{} ->
	    ?exception:get_message(InnerException);
	_Else -> 
	    "Request cannot be parsed or illegal"
    end;
get_exception_message(?exception_data(#network_exception{operation = Operation})) ->
    io_lib:format("Network exception at ~s", [Operation]);
get_exception_message(?exception_data(#extended_exception{})) ->
    undefined.

%%% tests                                   
