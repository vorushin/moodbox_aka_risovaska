-module(moodbox_context).

-export([create_context/1]).

-include("moodbox_facade.hrl").
-include("moodbox_context.hrl").
-include("moodbox_auth.hrl").

-define(auth, moodbox_auth).


create_context(undefined) ->
    #context{};
create_context(Header) ->
    #header{auth_ticket = TicketData} = Header,
    case ?auth:decode_ticket(TicketData) of
	undefined ->
	    #context{client_version_tag = Header#header.version_tag, client_version = Header#header.version, language = Header#header.language};
	Ticket ->
	    #context{user_id = Ticket#ticket.user_id, client_version_tag = Header#header.version_tag, client_version = Header#header.version, 
		     language = Header#header.language}
    end.
    
