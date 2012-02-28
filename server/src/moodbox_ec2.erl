-module(moodbox_ec2).

-export([run_instance/3, run_instance/5, terminate_instance/2, terminate_instances/2, describe_instances/1, describe_instances/2]).
-export([parse_describe_instances_response/1, parse_error/2]).

-define(BASE_URL, "https://ec2.amazonaws.com/?").
-define(datetime, moodbox_datetime).

-include("moodbox_ec2.hrl").

describe_instances(Keys) ->
	send_request(Keys, [{"Action", ["DescribeInstances"]}], {simple, parse_describe_instances_response}).
	
describe_instances(Keys, InstanceIds) ->
	send_request(Keys, [{"Action", ["DescribeInstances"]}, {"InstanceId", InstanceIds}], parse_describe_instances_response).

terminate_instance(Keys, InstanceId) ->
	terminate_instances(Keys, [InstanceId]).

terminate_instances(Keys, InstanceIds) ->
	send_request(Keys, [{"Action", ["TerminateInstances"]}, {"InstanceId", InstanceIds}], terminate_instance_parser).

run_instance(Keys, PubKeyName, ImageId) ->
    run_instance(Keys, PubKeyName, ImageId, undefined, undefined).

run_instance(Keys, PubKeyName, ImageId, InstanceType, UserData) ->
    run_instance(Keys, PubKeyName, ImageId, InstanceType, UserData, []).
run_instance(Keys, PubKeyName, ImageId, undefined = _InstanceType, undefined = _UserData, Properties) ->
    run_instance(Keys, PubKeyName, ImageId, Properties);
run_instance(Keys, PubKeyName, ImageId, InstanceType, UserData, Properties) when InstanceType =/= undefined ->
    run_instance(Keys, PubKeyName, ImageId, undefined, UserData, [{"InstanceType", [InstanceType]} | Properties]);
run_instance(Keys, PubKeyName, ImageId, InstanceType, UserData, Properties) when UserData =/= undefined ->
    run_instance(Keys, PubKeyName, ImageId, InstanceType, undefined, [{"UserData", [UserData]} | Properties]).

run_instance(Keys, PubKeyName, ImageId, AdditionalProps) ->
    send_request(Keys, [{"Action", ["RunInstances"]}, {"ImageId", [ImageId]}, {"MinCount", ["1"]}, {"MaxCount", ["1"]}, {"KeyName", [PubKeyName]}] ++ AdditionalProps, run_instance_parser).


%% parsing

-define(TagValue(TagName, Value), {TagName, _, [Value]}).
-define(TagList(TagName, Value), {TagName, _, Value}).

-define(ParseField(RecordName, ElementName, Field), parse_field(RecordName, ?TagValue(ElementName, Value), Result) -> setelement(#RecordName.Field, Result, Value)).
-define(ParseFieldWith(RecordName, ElementName, Field, Func), parse_field(RecordName, ?TagValue(ElementName, Value), Result) -> setelement(#RecordName.Field, Result, Func(Value))).
-define(ParseListField(RecordName, ElementName, Field, ItemRecord), parse_field(RecordName, ?TagList(ElementName, Value), Result) -> setelement(#RecordName.Field, Result, parse_list(ItemRecord, Value))).
-define(ParseRecordField(RecordName, ElementName, Field, Record), parse_field(RecordName, ?TagList(ElementName, Value), Result) -> setelement(#RecordName.Field, Result, parse_record(Record, Value))).

parse_describe_instances_response(Xml) ->
    {ok, ?TagValue("DescribeInstancesResponse", ?TagList("reservationSet", Elements)), _} = erlsom:simple_form(Xml, [{nameFun, fun(Name, _Namespace, _Prefix) -> Name end}]),
    parse_list(#ec2_reservation_info{}, Elements).

?ParseField(ec2_reservation_info, "reservationId", reservation_id);
?ParseField(ec2_reservation_info, "ownerId", owner_id);
?ParseListField(ec2_reservation_info, "groupSet", groups, #ec2_group{});
?ParseListField(ec2_reservation_info, "instancesSet", instances, #ec2_instance{});

?ParseFieldWith(ec2_group, "groupId", group_id, list_to_atom);

?ParseField(ec2_instance, "amiLaunchIndex", ami_launch_index);
?ParseField(ec2_instance, "dnsName", dns_name);
?ParseField(ec2_instance, "imageId", image_id);
?ParseField(ec2_instance, "instanceId", instance_id);
?ParseRecordField(ec2_instance, "instanceState", instance_state, #ec2_instance_state{});
?ParseFieldWith(ec2_instance, "instanceType", instance_type, list_to_atom);
?ParseField(ec2_instance, "keyName", key_name);
?ParseField(ec2_instance, "kernelId", kernel_id);
?ParseFieldWith(ec2_instance, "launchTime", launch_time, ?datetime:from_xsd);
?ParseRecordField(ec2_instance, "placement", placement, #ec2_placement{});
?ParseField(ec2_instance, "privateDnsName", private_dns_name);
?ParseListField(ec2_instance, "productCodes", product_codes, #ec2_product_code{});
?ParseField(ec2_instance, "ramdiskId", ramdisk_id);
?ParseField(ec2_instance, "reason", reason);

?ParseFieldWith(ec2_instance_state, "code", code, list_to_integer);
?ParseFieldWith(ec2_instance_state, "name", name, list_to_atom);

?ParseField(ec2_placement, "availabilityZone", availability_zone);

?ParseField(ec2_product_code, "productCode", product_code);

parse_field(_, _, Result) -> % ignore any unknown elements
    Result.


parse_list(ItemRecord, Elements) ->
    parse_list(ItemRecord, Elements, []).
parse_list(_ItemRecord, [] = _Elements, Result) ->
    lists:reverse(Result);
parse_list(ItemRecord, [?TagList("item", Elements) | Tail] = _Elements, Result) ->
    Value = parse_record(ItemRecord, Elements),
    parse_list(ItemRecord, Tail, [Value | Result]).

parse_record(Record, Elements) ->
    parse_record(element(1, Record), Elements, Record).
parse_record(_RecordName, [] = _Elements, Result) ->
    Result;
parse_record(RecordName, [Element | Tail] = _Elements, Result) ->
    NewResult = parse_field(RecordName, Element, Result),
    parse_record(RecordName, Tail, NewResult).




parse_error({startElement, _, "Error", _, _}, {_Current, _Flag, All}) ->
    {[], [], All};
parse_error({startElement, _, "Code", _, _}, {Current, _Flag, All}) ->
    {Current, code, All};
parse_error({startElement, _, "Message", _, _}, {Current, _Flag, All}) ->
	{Current, message, All};
parse_error({endElement, _, "Error", _}, {Current, _Flag, All}) ->
	{[], [], lists:append(All, [Current])};
parse_error({endElement, _, "Errors", _}, {_Current, _Flag, All}) ->
	{error, lists:filter(fun is_not_empty/1, lists:reverse(All))};
parse_error({characters, Text}, {Current, Flag, All}) when is_atom(Flag) ->
	Current1 = lists:append(Current, [{Flag, Text}]),
	{Current1, Flag, All};		
parse_error(_Event, State) ->
	State.

is_not_empty(Value) ->
	length(Value) > 0.


%% util

parse_response(Response, Parser) ->
    {{_, _, _}, _, Body} = Response,

    case Parser of
	undefined ->
	    Response;
	{simple, ParserFun} ->
	    ?MODULE:ParserFun(Body);
	_ ->
	    erlsom:sax(Body, {[], [], []}, determine_parser(Parser, Body))
    end.
	

determine_parser(Parser, Response) when is_list(Response) ->
    ErrorPos = string:str(Response, "Errors"),
    if
	ErrorPos > 0 ->
	    parse_error;
	true ->
	    Parser
    end.

prepare(Keys, Params) ->
    P1 = format_param_list(lists:flatten([Params, generate_default_params(get_key_id(Keys))])),
    P2 = P1 ++ [{"Signature", generate_checksum(Keys, P1)}],
    ?BASE_URL ++ encode(P2).

send_request(Keys, Params, Parser) ->
    send_request(Keys, Params, Parser, undefined).

send_request(Keys, Params, Parser, undefined = _PrepareFun) ->
    Req = prepare(Keys, Params),
    case http:request(get, {Req, []}, [{ssl, []}], []) of
	{ok, Response} ->
	    parse_response(Response, Parser);
	Error ->
	    Error
    end;
send_request(Keys, Params, Parser, PrepareFun) ->
    Req = prepare(Keys, Params),
    case http:request(get, {Req, []}, [{ssl, []}], []) of
	{ok, Response} ->
	    PrepareFun(Response),
	    parse_response(Response, Parser);
	Error ->
	    PrepareFun(Error),
	    Error
    end.

generate_default_params(Identifier) ->
    [{"AWSAccessKeyId", [Identifier]}, {"Timestamp", [get_timestamp()]}, {"SignatureVersion", ["1"]}, {"Version", ["2008-05-05"]}].

format_param_list(Params) ->
    format_param_list(Params, []).

format_param_list([{Name, Values} | Rest], Accum) when is_list(Values), length(Values) > 1 ->
    format_param_list(Rest, process_mult_values(Name, Values, 1, Accum));

format_param_list([{Name, Value} | Rest], Accum) when is_list(Value), length(Value) == 1 ->
    [V] = Value,
    format_param_list(Rest, Accum ++ [{Name, V}]);

format_param_list([], Accum) ->
    Accum.

process_mult_values(Name, [H|T], Pos, Accum) ->
    process_mult_values(Name, T, Pos + 1, Accum ++ [{lists:flatten([Name, ".", integer_to_list(Pos)]), H}]);

process_mult_values(_Name, [], _Pos, Accum) ->
    Accum.

generate_checksum(Keys, Params) ->
    F = fun(F, S) ->
		{N1, _V1} = F,
		{N2, _V2} = S,
		string:to_lower(N1) < string:to_lower(N2)
	end,
    NP = normalize_params(lists:sort(F, Params)),
    sign_request(Keys, NP).

get_timestamp() ->
    ?datetime:to_xsd(?datetime:now()).

normalize_params(Params) ->
    F = fun({Name, Value}) ->
		Name ++ Value
	end,
    lists:flatten(lists:map(F, Params)).

get_key_id({KeyId, _Key} = _Keys) ->
	KeyId.
sign_request({_KeyId, Key} = _Keys, Req) ->
    base64:encode_to_string((crypto:sha_mac(Key, Req))).


-define(PERCENT, 37).  % $\% 
-define(FULLSTOP, 46). % $\.
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
		    (C >= $a andalso C =< $f) orelse
		    (C >= $A andalso C =< $F))).
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
		     (C >= $A andalso C =< $Z) orelse
		     (C >= $0 andalso C =< $9) orelse
		     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
		      C =:= $_))).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

escape(Atom) when is_atom(Atom) ->
    escape(atom_to_list(Atom));
escape(Int) when is_integer(Int) ->
    escape(integer_to_list(Int));
escape(String) ->
    escape(String, []).

escape([], Acc) ->
    lists:reverse(Acc);
escape([C | Rest], Acc) when ?QS_SAFE(C) ->
    escape(Rest, [C | Acc]);
escape([$\s | Rest], Acc) ->
    escape(Rest, [$+ | Acc]);
escape([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    escape(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).

revjoin([], _Separator, Acc) ->
    Acc;
revjoin([S | Rest], Separator, []) ->
    revjoin(Rest, Separator, [S]);
revjoin([S | Rest], Separator, Acc) ->
    revjoin(Rest, Separator, [S, Separator | Acc]).  

encode(Props) ->
    RevPairs = lists:foldl(fun ({K, V}, Acc) ->
				   [[escape(K), $=, escape(V)] | Acc]
			   end, [], Props),
    lists:flatten(revjoin(RevPairs, $&, [])).
