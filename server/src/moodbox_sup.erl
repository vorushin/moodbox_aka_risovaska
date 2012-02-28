%%%-------------------------------------------------------------------
%%% @doc    Moodbox application supervisor module.
%%% @end -------------------------------------------------------------

-module(moodbox_sup).
-behavior(supervisor).

%% External exports
-export([start_link/1]).
%% Supervisor callbacks
-export([init/1]).

-include("moodbox.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link(Mode) ->
    supervisor:start_link({local, moodbox_sup}, moodbox_sup, Mode).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

init(Mode) ->
    RestartStrategy    = one_for_all,
    MaxRestarts        = 10,
    MaxTimeBetRestarts = 3000,
    SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
    
    % commented possibility to start other libraries under our supervisor:
    %child_def(ssl_sup, start_link, [], supervisor),
    %child_def(crypto_sup, start_link, [], supervisor), 
    %child_def(mnesia_sup, start, [], supervisor),
    case Mode of
	?SINGLE_SERVER ->
	    ChildSpecs = [child_def(mnesia_sup, start, [], worker, 30000), 
			  child_def(moodbox_server, start_link, [], worker, 60000), 
			  child_def(moodbox_notification, start_link, [], worker, 60000)];
	?NOTIFICATION_SERVER ->
	    ChildSpecs = [child_def(mnesia_sup, start, [], worker, 30000),
			  child_def(moodbox_server, start_link, [], worker, 60000),
			  child_def(moodbox_notification, start_link, [], worker, 60000)];
	?SERVER_WITH_DB ->
	    ChildSpecs = [child_def(mnesia_sup, start, [], worker, 30000), 
			  child_def(moodbox_server, start_link, [], worker, 60000)];
	?SERVER ->
	    ChildSpecs = [child_def(mnesia_sup, start, [], worker, 30000),
			  child_def(moodbox_server, start_link, [], worker, 60000),
			  child_def(moodbox_notification, start_link, [], worker, 60000)];
	?DB_SERVER ->
	    ChildSpecs = [child_def(mnesia_sup, start, [], worker, 90000)];
	?LOAD_BALANCER ->
	    ChildSpecs = [child_def(moodbox_nginx, start_link, [], worker, 2000)];
	_Other ->
	    ChildSpecs = [],
	    exit(io_lib:format("Wrong Mode: ~p", [Mode]))
    end,    
    {ok, {SupFlags, ChildSpecs}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

child_def(Mod, Function, Parameters, Type, TimeOut) ->
    {Mod,
     {Mod, Function, Parameters},
     permanent,
     TimeOut,
     Type,
     [Mod]}.
