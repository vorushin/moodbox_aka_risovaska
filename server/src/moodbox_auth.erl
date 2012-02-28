-module(moodbox_auth).

-export([get_encoded_ticket/1, is_valid_ticket/1, create_ticket/1, encode_ticket/1, decode_ticket/1]).

-include("moodbox_auth.hrl").
-include("moodbox_config.hrl").

-define(datetime, moodbox_datetime).

%%% exceptions
-export([get_exception_message/1]).
-include("moodbox_exception.hrl").


get_encoded_ticket(UserId) ->
    encode_ticket(create_ticket(UserId)).

is_valid_ticket(#ticket{user_id = UserId, time_stamp = TimeStamp} = _Ticket) when UserId =/= undefined ->
    Now = ?datetime:now(),
    TimeStamp < Now andalso ?datetime:add(TimeStamp, ?AUTH_TICKET_TIME_TO_LIVE) > Now;
is_valid_ticket(_Ticket) ->
    false.

create_ticket(UserId) ->
    #ticket{user_id = UserId, time_stamp = ?datetime:now(), random = random:uniform(16#FFFFFFFFFFFFFFFF)}. % @todo seed random

encode_ticket(Ticket) ->
    crypto:aes_cfb_128_encrypt(?AES_KEY, ?AES_VECTOR, term_to_binary([1, Ticket#ticket.random, 2, Ticket#ticket.time_stamp, 3, Ticket#ticket.user_id])).

decode_ticket(undefined = _TicketData) ->
    undefined;
decode_ticket(TicketData) ->
    try
	Ticket = restore_ticket(binary_to_term(crypto:aes_cfb_128_decrypt(?AES_KEY, ?AES_VECTOR, TicketData))),
	case is_valid_ticket(Ticket) of
	    true ->
		Ticket;
	    false ->
		?error(#bad_auth_exception{})
	end
    catch
	ExceptionType:Exception ->
	    ?error_inner(#bad_auth_exception{}, ExceptionType, Exception)
    end.

restore_ticket(TicketData) ->
    restore_ticket(TicketData, #ticket{} ).

restore_ticket([], Ticket) ->
    Ticket;
restore_ticket([FieldId, Value | Rest], Ticket) ->
    if 
	FieldId == 1 -> 
	    NewTicket = Ticket#ticket{random = Value};
	FieldId == 2 -> 
	    NewTicket = Ticket#ticket{time_stamp = Value};
	FieldId == 3 -> 
	    NewTicket = Ticket#ticket{user_id = Value}
    end,
    restore_ticket(Rest, NewTicket).


%%% exceptions handling

get_exception_message(?exception_data(#bad_auth_exception{})) ->
    "Authentication info is missed or invalid".
