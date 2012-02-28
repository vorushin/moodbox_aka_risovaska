-module(moodbox_request).

-export([server_call_test/3, server_call_test/5, server_call_std_model/3, server_call_std_model/4, server_call/4, server_call/5]).
-export([server_erl_call/3]).

-include("moodbox_config.hrl").
-include("moodbox_facade.hrl").
-include("moodbox_database.hrl").
-include("moodbox_context.hrl").

-define(model_definition, moodbox_model_definition).
-define(model, moodbox_model).
-define(parser, moodbox_xml_parser).
-define(generator, moodbox_xml_generator).

server_call_test(Url, Func, Args) ->
    server_call_std_model(Url, Func, Args).
server_call_test(Url, Login, Password, Func, Args) ->
    {ok, #auth_ticket_result{auth_ticket = Ticket, result_code = ok}} = server_call_test(Url, get_auth_ticket, [Login, Password]),
    server_call_std_model(Url, #header{auth_ticket = Ticket}, Func, Args).

server_call_std_model(Url, Func, Args) ->
    server_call_std_model(Url, undefined, Func, Args).
server_call_std_model(Url, Header, Func, Args) ->
    server_call(?model:create(?model_definition:get_definition()), Url, Header, Func, Args).

server_call(Model, Url, Func, Args) ->
    server_call(Model, Url, undefined, Func, Args).
server_call(Model, Url, Header, Func, Args) ->
    CallEnvelope = #envelope{header = Header, body = list_to_tuple([Func | Args])},
    %io:format("CallEnvelope: ~w~n", [CallEnvelope]),
    
    % prepare XML
    Data = ?generator:generate(Model, envelope, CallEnvelope, fun collect/2, <<>>),

    Reply = http:request(post, {Url, [], "text/xml", Data}, [{autoredirect, true}, {ssl, [{verify, 0}]}], 
			 [{sync,true}, {headers_as_is,true}, {body_format, binary}]),

    case handle_reply(Reply) of
	{ok, _Headers, RawResponse} ->
	    ResponseData = ?parser:parse(Model, envelope, RawResponse),
	    %io:format("ResponseData: ~p~n", [ResponseData]),

	    % extract header and Response components
	    #envelope{header = _Header, body = Response} = ResponseData,
	    %io:format("HeaderData: ~w, Response: ~w~n", [Header, Response]),

	    if
		is_record(Response, fault) ->
		    {fault, Response};

		is_tuple(Response) andalso tuple_size(Response) =:= 2 ->
		    {ok, element(2, Response)}
	    end;

	{error, Code, Phrase} ->
	    {error, Code, Phrase}
    end.


handle_reply(Reply) ->
    case Reply of
 	{ok, {{_HttpVersion, Code, _ReasonPhrase}, ResponseHeaders, 
	      ResponseBody }} when Code=:=200; Code=:=204 -> 
 	    {ok, ResponseHeaders, ResponseBody};
	
	{ok, {{_HttpVersion, Code, ReasonPhrase}, _ResponseHeaders, 
	      _ResponseBody }} ->
	    {error, Code, ReasonPhrase}
    end.

collect(Bin, Collected) ->
    %io:format("~s", [binary_to_list(Bin)]),
    << Collected/binary, Bin/binary >>.


server_erl_call(Url, Func, Args) ->
    Data = encrypt(term_to_binary({Func, Args})),

    Reply = http:request(post, {Url, [], "application/octet-stream", Data}, [{autoredirect, true}, {ssl, [{verify, 0}]}], 
			 [{sync,true}, {headers_as_is,true}, {body_format, binary}]),

    case handle_reply(Reply) of
	{ok, _Headers, RawResponse} ->
	    Result = binary_to_term(decrypt(RawResponse)),
	    %io:format("Result: ~p~n", [Result]),

	    Result;

	{error, Code, Phrase} ->
	    {error, Code, Phrase}
    end.

encrypt(Data) ->
    crypto:aes_cfb_128_encrypt(?AES_KEY, ?AES_VECTOR, Data).
decrypt(Data) ->
    crypto:aes_cfb_128_decrypt(?AES_KEY, ?AES_VECTOR, Data).
