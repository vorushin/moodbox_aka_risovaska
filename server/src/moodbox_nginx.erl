-module(moodbox_nginx).

-behavior(gen_server).

%% gen_server callbacks
-export([start_link/0, start/0, stop/1, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
%% server's API
-export([get_config/0, update_config/1, get_balanced_addresses/1, create_config/1]).

-include("moodbox_config.hrl").
-include_lib("mochiweb/include/mochiweb.hrl").

-define(ServerLineTemplate, "server ~s;\n").
-define(ConfigPath, "priv/nginx/backend.conf").
-define(RemoteFunctions, [get_config, update_config]).

-define(safe_try(Expr), try Expr catch _:_ -> undefined end).
-define(network_try(Expr, ExceptionTypeVar, ExceptionVar), try Expr catch ExceptionTypeVar:ExceptionVar -> on_network_error(ExceptionTypeVar, ExceptionVar) end).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start(?MODULE, [], []).

stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API

get_config() ->
    file:read_file(?ConfigPath).
update_config(Data) ->
    file:write_file(?ConfigPath, Data).

get_balanced_addresses(Config) ->
    Content = binary_to_list(Config),
    case re:run(Content, "^\\s*server\\s+([^\\s;]+).*$", [global, {capture, [1], list}, multiline, {newline, anycrlf}]) of
	{match, Servers} ->
	    lists:map(fun([Item]) -> Item end, Servers);
	nomatch ->
	    []
    end.

create_config(Addresses) ->
    lists:flatten(io_lib:format("upstream  backend  {\n"
				"    ~s"
				"}\n", [get_config_lines(Addresses)])).

get_config_lines(Addresses) ->
    get_config_lines(Addresses, []).
get_config_lines([] = _Addresses, Result) ->
    lists:reverse(Result);
get_config_lines([Item | Tail] = _Addresses, Result) ->
    get_config_lines(Tail, [io_lib:format(?ServerLineTemplate, [Item]) | Result]).

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks functions for gen_server

init(_Args) ->
    process_flag(trap_exit, true),

    % define request handler
    Handler = fun(Req) -> handle(Req) end,

    % start mochiweb
    Ssl = #ssl{keyfile = ?SSL_KEYFILE, certfile = ?SSL_CERTFILE, verify = ?SSL_VERIFY, password = ?SSL_PASSWORD, cacertfile = ?SSL_CACERTFILE, 
	       ciphers = ?SSL_CIPHERS},
    mochiweb_http:start([{loop, Handler}, {port, 8443}, {ssl, Ssl}]),

%    if 
%	Port == 443 ->
%	    Ssl = #ssl{keyfile = ?SSL_KEYFILE, certfile = ?SSL_CERTFILE, verify = ?SSL_VERIFY, password = ?SSL_PASSWORD, cacertfile = ?SSL_CACERTFILE, 
%		       ciphers = ?SSL_CIPHERS},
%	    mochiweb_http:start([{loop, Handler}, {port, Port}, {ssl, Ssl}]);	
%	true ->
%	    mochiweb_http:start([{loop, Handler}, {port, Port}])
%    end,

    {ok, undefined}.

handle_call(Unknown, _From, State) ->
    {reply, Unknown, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Unknown, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _Pid, Reason}, State) ->
    ?safe_try(error_logger:error_report([{type, notification_internal_process}, {reason, Reason}, {trace, erlang:get_stacktrace()}])),
    {noreply, State};
handle_info(_Message, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    mochiweb_http:stop(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal processes


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions

-ifdef(DEBUG).
-define(GetStackTrace, ?safe_try(erlang:get_stacktrace())).
-else.
-define(GetStackTrace, undefined).
-endif.

handle(Req) ->
    try
	[$/ | Path] = Req:get(path),
	%io:format("Path: ~p~n", [Path]),
	case Path of
	    "do" ->
		case parse_request(Req) of
		    {error, _Error} = ErrorData ->
			respond(Req, ErrorData);
		    {ok, Func, Args} ->
			case lists:member(Func, ?RemoteFunctions) of
			    false ->
				respond(Req, {error, unknown_function});
			    true ->
				Result = 
				    try
					{ok, apply(?MODULE, Func, Args)}
				    catch
					ExceptionType1:Exception1 ->
					    {fault, ExceptionType1, Exception1, ?GetStackTrace}
				    end,
				respond(Req, Result)
			end
		end;
	    _ ->
		?network_try(Req:not_found(), ExceptionType2, Exception2)
	end
    catch
	ExceptionType:Exception ->
	    on_exception(Req, ExceptionType, Exception)
    end.

parse_request(Req) ->
    try
	RawData = Req:recv_body(),
	{Func, Args} = binary_to_term(decrypt(RawData)),
	{ok, Func, Args}
    catch
	_:_ ->
	    {error, parse_error}
    end.

respond(Req, Data) ->
    ?network_try(Req:ok({"application/octet-stream", encrypt(term_to_binary(Data))}), ExceptionType, Exception).

-ifdef(DEBUG).
-define(LogNetworkError, ?safe_try(error_logger:error_report([{module, ?MODULE}, {type, ExceptionType}, {error, Exception}, {network_error, error}, {trace, ?safe_try(erlang:get_stacktrace())}]))).
-else.
-define(LogNetworkError, ok).
-endif.

on_network_error(ExceptionType, Exception) ->
    ?LogNetworkError.

on_exception(Req, ExceptionType, Exception) ->
    ?safe_try(error_logger:error_report([{module, ?MODULE}, {type, ExceptionType}, {error, Exception}, {trace, ?safe_try(erlang:get_stacktrace())}])),
    respond(Req, {fault, internal_error}).

encrypt(Data) ->
    crypto:aes_cfb_128_encrypt(?AES_KEY, ?AES_VECTOR, Data).
decrypt(Data) ->
    crypto:aes_cfb_128_decrypt(?AES_KEY, ?AES_VECTOR, Data).
