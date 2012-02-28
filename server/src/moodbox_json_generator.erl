-module(moodbox_json_generator).

-export([generate/5]).

-include_lib("eunit/include/eunit.hrl").
-include("moodbox_model.hrl").

-define(model, moodbox_model).
-define(datetime, moodbox_datetime).
-define(test_model_definition, moodbox_model_test_definition).
-define(type_property_name, "_Type__").

%%% exceptions
%-export([get_exception_message/1]).
%-include("moodbox_exception.hrl").
%-record(simple_value_in_union, {union_type, value}). % currently not used

% @todo don't generate "someField":null pairs?
% @todo cache normally cased names to make it faster

generate(Model, DocumentRootTypeName, Data, WriteFunc, WriteState) ->
    write(Model,  #union{types = [DocumentRootTypeName]}, Data, WriteFunc, WriteState).

write(_Model, _Type, undefined, WriteFunc, WriteState) ->
    WriteFunc(<< "null" >>, WriteState);
write(_Model, string, Value, WriteFunc, WriteState) ->
    WriteState1 = WriteFunc(<< $\" >>, WriteState),
    WriteState2 = WriteFunc(json_escape(Value), WriteState1),
    WriteFunc(<< $\" >>, WriteState2);
write(_Model, int, Value, WriteFunc, WriteState) ->
    WriteFunc(list_to_binary(integer_to_list(Value)), WriteState);
write(Model, atom, Value, WriteFunc, WriteState) ->
    write(Model, string, atom_to_list(Value), WriteFunc, WriteState);
write(_Model, float, Value, WriteFunc, WriteState) ->
    WriteFunc(list_to_binary(mochinum:digits(Value)), WriteState);
write(_Model, bool, Value, WriteFunc, WriteState) ->
    WriteFunc(list_to_binary(atom_to_list(Value)), WriteState);
write(Model, datetime, Value, WriteFunc, WriteState) ->
    WriteState1 = write_object_start(Model, "Date", WriteFunc, WriteState),
    WriteState2 = WriteFunc(<< $, >>, WriteState1),
    WriteState3 = write_pair(Model, value, string, ?datetime:to_xsd(Value), WriteFunc, WriteState2),
    write_object_end(WriteFunc, WriteState3);
write(_Model, binary, Value, WriteFunc, WriteState) ->
    WriteState1 = WriteFunc(<< $\" >>, WriteState),
    WriteState2 = WriteFunc(base64:encode(Value), WriteState1),
    WriteFunc(<< $\" >>, WriteState2);
write(Model, #list{type = ItemsType}, Value, WriteFunc, WriteState) when is_list(Value) ->
    write_list(Model, ItemsType, Value, WriteFunc, WriteState);
write(Model, #union{types = _UnionTypes}, Value, WriteFunc, WriteState) when is_tuple(Value) ->
    % @todo check type
    write(Model, element(1, Value), Value, WriteFunc, WriteState);
write(Model, Type, Value, WriteFunc, WriteState) ->
    TypeInfo = ?model:get_type_info(Model, Type),
    case TypeInfo of
	#enum{} ->
	    % @todo check value
	    write(Model, string, atom_to_list(Value), WriteFunc, WriteState);
	#struct{} ->
	    write_struct(Model, TypeInfo, Value, WriteFunc, WriteState)
    end.

write_list(Model, ItemsType, Value, WriteFunc, WriteState) ->
    WriteState1 = WriteFunc(<< $[ >>, WriteState),
    WriteState2 = write_list_items(Model, ItemsType, Value, true, WriteFunc, WriteState1),
    WriteFunc(<< $] >>, WriteState2).    
write_list_items(_Model, _ItemsType, [] = _Value, _IsFirst, _WriteFunc, WriteState) ->
    WriteState;
write_list_items(Model, ItemsType, [Item | Rest] = _Value, IsFirst, WriteFunc, WriteState) ->
    % @todo support simplified form of list of unions (omit <li> for unions)
    WriteState1 = if IsFirst -> WriteState; true -> WriteFunc(<< $, >>, WriteState) end,
    WriteState2 = write(Model, ItemsType, Item, WriteFunc, WriteState1),
    write_list_items(Model, ItemsType, Rest, false, WriteFunc, WriteState2).

write_struct(Model, TypeInfo, Value, WriteFunc, WriteState) ->
    WriteState1 = write_object_start(Model, TypeInfo#struct.name, WriteFunc, WriteState),
    WriteState2 = write_struct(Model, TypeInfo#struct.fields, 1, Value, WriteFunc, WriteState1),
    write_object_end(WriteFunc, WriteState2).
write_struct(_Model, [] = _FieldsInfo, _Index, _Value, _WriteFunc, WriteState) ->
    WriteState;
write_struct(Model, [FieldInfo | RestFieldsInfo], Index, Value, WriteFunc, WriteState) ->
    #field{name = FieldName, type = FieldType} = FieldInfo,
    WriteState1 = WriteFunc(<< $, >>, WriteState),
    WriteState2 = write_pair(Model, FieldName, FieldType, element(Index + 1, Value), WriteFunc, WriteState1),
    write_struct(Model, RestFieldsInfo, Index + 1, Value, WriteFunc, WriteState2).


write_object_start(WriteFunc, WriteState) ->
    WriteFunc(<< ${ >>, WriteState).
write_object_start(Model, Type, WriteFunc, WriteState) ->
    WriteState1 = write_object_start(WriteFunc, WriteState),
    TypeName = if is_list(Type) -> Type; true -> ?model:to_pascal_case(atom_to_list(Type)) end,
    write_pair(Model, ?type_property_name, string, TypeName, WriteFunc, WriteState1).
write_object_end(WriteFunc, WriteState) ->
    WriteFunc(<< $} >>, WriteState).

write_pair(Model, Property, Type, Value, WriteFunc, WriteState) ->
    PropertyName = if is_list(Property) -> Property; true -> ?model:to_camel_case(atom_to_list(Property)) end,
    WriteState1 = write(Model, string, PropertyName, WriteFunc, WriteState),
    WriteState2 = WriteFunc(<< ":" >>, WriteState1),
    write(Model, Type, Value, WriteFunc, WriteState2).
    

%% escapes string for XML
json_escape(Chars) ->
    json_escape(Chars, <<>>).
json_escape([], Result) ->
    Result;
json_escape([IoList | Chars], Result) when is_list(IoList) ->
  json_escape(Chars, << Result/binary, (json_escape(IoList))/binary >>);
json_escape([Char | Chars], Result) ->
  json_escape(Chars, << Result/binary, (escape_char(Char))/binary >>).


% @todo also escape other control chars using \uXXXX
escape_char($\") -> << "\\\"" >>;
escape_char($\\) -> << "\\\\" >>;
escape_char($\b) -> << "\\b" >>;
escape_char($\n) -> << "\\n" >>;
escape_char($\r) -> << "\\r" >>;
escape_char($\t) -> << "\\t" >>;
escape_char($\f) -> << "\\f" >>;
escape_char(Char) -> list_to_binary(xmerl_ucs:to_utf8(Char)).


%%% exceptions handling

%get_exception_message(?exception_data(#simple_value_in_union{union_type = UnionType, value = Value})) ->
%    io_lib:format("Simple values cannot be generated as union members, but value ~p found in union ~p", [Value, UnionType]).


%%% test functions

test_write_func(Bin, State) ->
    io:format("~s", [binary_to_list(Bin)]),
    << State/binary, Bin/binary >>.

get_test_model() ->
    ?model:create(?test_model_definition:get_definition()).

test_generate(RootType, Data) ->
    generate(get_test_model(), RootType, Data, fun test_write_func/2, <<>>).

simple_types_test_()->
    [?_assertMatch( << "{\"_Type__\":\"Simpletypes\",\"atom\":\"atom1\",\"i\":11,\"f\":1.1,\"bin\":\"MTIz\",\"b\":true,\"s\":\"test\\n\\\"text\\\"\",\"d\":{\"_Type__\":\"Date\",\"value\":\"2008-07-25T15:33:22.000Z\"},\"e\":\"atom3\",\"t\":{\"_Type__\":\"B\",\"c\":\"some string\"},\"c\":\"empty\",\"o\":null}" >>, 
      test_generate(simpletypes, 
             {simpletypes, atom1, 11, 1.1, <<"123">>, true, "test\n\"text\"",
             1217000002000000, atom3,
             {b, "some string"},
             "empty", undefined} )) 
    ].

 list_test_() ->
    [?_assertMatch(<< "{\"_Type__\":\"UnionAndList\",\"u\":null,\"u2\":null,\"u3\":null,\"u4\":null,\"u5\":null,\"l\":[\"Li1\",\"Li2\",\"Li3\"],\"l2\":[\"S1\",\"S2\",\"S3\"],\"l3\":[true,false,true],\"l4\":[[1,2,3],[4,5,6],[7,8,0]],\"lu\":null,\"b\":{\"_Type__\":\"B\",\"c\":\"true\"}}" >>, 
     test_generate(unionAndList, 
	       {unionAndList, undefined,
               undefined, undefined, undefined, undefined,
               ["Li1", "Li2", "Li3"],
               ["S1", "S2", "S3"],
               [true, false, true],
               [[1, 2, 3], [4, 5, 6], [7, 8, 0]],
               undefined,
               {b, "true"}} ))
     ].

envelope_test_() ->
    [?_assertMatch(<< "{\"_Type__\":\"Envelope\",\"header\":{\"_Type__\":\"Header\",\"authTicket\":\"MTIz\"},\"body\":{\"_Type__\":\"GetAuthTicket\",\"login\":\"test1\",\"password\":\"topsecret\"}}" >>, 
      test_generate(envelope, 
	  {envelope, {header, <<"123">>},
          {getAuthTicket, "test1", "topsecret"}} )) 
    ].

fault_test_() ->
    [].
