-module(moodbox_xml_generator).

-export([generate/5]).

-include_lib("eunit/include/eunit.hrl").
-include("moodbox_model.hrl").

-define(model, moodbox_model).
-define(datetime, moodbox_datetime).
-define(test_model_definition, moodbox_model_test_definition).

%%% exceptions
%-export([get_exception_message/1]).
%-include("moodbox_exception.hrl").
%-record(simple_value_in_union, {union_type, value}). % currently not used


generate(Model, DocumentRootTypeName, Data, WriteFunc, WriteState) ->
    write(Model,  #union{types = [DocumentRootTypeName]}, Data, WriteFunc, WriteState).

write(_Model, string, Value, WriteFunc, WriteState) ->
    WriteFunc(xml_escape(Value), WriteState);
write(_Model, int, Value, WriteFunc, WriteState) ->
    WriteFunc(list_to_binary(integer_to_list(Value)), WriteState);
write(_Model, atom, Value, WriteFunc, WriteState) ->
    WriteFunc(list_to_binary(atom_to_list(Value)), WriteState);
write(_Model, float, Value, WriteFunc, WriteState) ->
    WriteFunc(list_to_binary(mochinum:digits(Value)), WriteState);
write(_Model, bool, Value, WriteFunc, WriteState) ->
    WriteFunc(list_to_binary(atom_to_list(Value)), WriteState);
write(_Model, datetime, Value, WriteFunc, WriteState) ->
    WriteFunc(list_to_binary(?datetime:to_xsd(Value)), WriteState);
write(_Model, binary, Value, WriteFunc, WriteState) ->
    WriteFunc(base64:encode(Value), WriteState);
write(Model, #list{type = ItemsType}, Value, WriteFunc, WriteState) when is_list(Value) ->
    write_list(Model, ItemsType, Value, WriteFunc, WriteState);
write(Model, #union{types = _UnionTypes}, Value, WriteFunc, WriteState) when is_tuple(Value) ->
    % @todo check type
    TypeInfo = ?model:get_type_info(Model, element(1, Value)),
    Tag = TypeInfo#struct.name,
    WriteState1 = write_start_tag(Tag, WriteFunc, WriteState),
    WriteState2 = write_struct(Model, TypeInfo, Value, WriteFunc, WriteState1),
    write_end_tag(Tag, WriteFunc, WriteState2);
write(Model, Type, Value, WriteFunc, WriteState) ->
    TypeInfo = ?model:get_type_info(Model, Type),
    case TypeInfo of
	#enum{} ->
	    % @todo check value
	    WriteFunc(list_to_binary(atom_to_list(Value)), WriteState);
	#struct{} ->
	    write_struct(Model, TypeInfo, Value, WriteFunc, WriteState)
    end.

write_list(_Model, _ItemsType, [] = _Value, _WriteFunc, WriteState) ->
    WriteState;
write_list(Model, ItemsType, [Item | Rest] = _Value, WriteFunc, WriteState) ->
    % @todo support simplified form of list of unions (omit <li> for unions)
    NewWriteState = write_in_tags(li, Model, ItemsType, Item, WriteFunc, WriteState),
    write_list(Model, ItemsType, Rest, WriteFunc, NewWriteState).

write_struct(Model, TypeInfo, Value, WriteFunc, WriteState) ->
    write_struct(Model, TypeInfo#struct.fields, 1, Value, WriteFunc, WriteState).
write_struct(_Model, [] = _FieldsInfo, _Index, _Value, _WriteFunc, WriteState) ->
    WriteState;
write_struct(Model, [FieldInfo | RestFieldsInfo], Index, Value, WriteFunc, WriteState) ->
    #field{name = FieldName, type = FieldType} = FieldInfo,
    NewWriteState = write_in_tags(FieldName, Model, FieldType, element(Index + 1, Value), WriteFunc, WriteState),
    write_struct(Model, RestFieldsInfo, Index + 1, Value, WriteFunc, NewWriteState).


write_in_tags(Tag, Model, Type, Value, WriteFunc, WriteState) ->
    if
	Value == undefined ->
	    case ?model:get_type_info_silently(Model, Type) of
		#enum{} ->
		    WriteState1 = write_start_tag(Tag, WriteFunc, WriteState),
		    WriteState2 = WriteFunc(<< "undefined" >>, WriteState1),
		    write_end_tag(Tag, WriteFunc, WriteState2);
		_Other ->
		    write_empty_tag(Tag, true, WriteFunc, WriteState)
	    end;
	true ->
	    WriteState1 = write_start_tag(Tag, WriteFunc, WriteState),
	    WriteState2 = write(Model, Type, Value, WriteFunc, WriteState1),
	    write_end_tag(Tag, WriteFunc, WriteState2)
    end.

write_start_tag(Atom, WriteFunc, WriteState) ->
    WriteState1 = WriteFunc(<< $< >>, WriteState),
    WriteState2 = WriteFunc(list_to_binary(atom_to_list(Atom)), WriteState1),
    WriteFunc(<< $> >>, WriteState2).
write_end_tag(Atom, WriteFunc, WriteState) ->
    WriteState1 = WriteFunc(<< "</" >>, WriteState),
    WriteState2 = WriteFunc(list_to_binary(atom_to_list(Atom)), WriteState1),
    WriteFunc(<< $> >>, WriteState2).
write_empty_tag(Atom, IsNull, WriteFunc, WriteState) ->
    WriteState1 = WriteFunc(<< $< >>, WriteState),
    WriteState2 = WriteFunc(list_to_binary(atom_to_list(Atom)), WriteState1),
    WriteState3 = if IsNull -> WriteFunc(<< " isNull=\"true\"" >>, WriteState2); true -> WriteState2 end,
    WriteFunc(<< "/>" >>, WriteState3).


%% escapes string for XML
xml_escape(Chars) ->
    xml_escape(Chars, <<>>).
xml_escape([], Result) ->
    Result;
xml_escape([IoList | Chars], Result) when is_list(IoList) ->
  xml_escape(Chars, << Result/binary, (xml_escape(IoList))/binary >>);
xml_escape([Char | Chars], Result) ->
  xml_escape(Chars, << Result/binary, (escape_char(Char))/binary >>).

escape_char(38) -> << "&amp;" >>;
escape_char(34) -> << "&quot;" >>;
escape_char(60) -> << "&lt;" >>;
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
    [?_assertMatch( << "<simpletypes><atom>atom1</atom><i>11</i><f>1.1</f><bin>MTIz</bin><b>true</b><s>test text</s><d>2008-07-25T15:33:22.000Z</d><e>atom3</e><t><c>some string</c></t><c>empty</c><o isNull=\"true\"/></simpletypes>" >>, 
      test_generate(simpletypes, 
             {simpletypes, atom1, 11, 1.1, <<"123">>, true, "test text",
             1217000002000000, atom3,
             {b, "some string"},
             "empty", undefined} )) 
    ].

 list_test_() ->
    [?_assertMatch(<< "<unionAndList><u isNull=\"true\"/><u2 isNull=\"true\"/><u3 isNull=\"true\"/><u4 isNull=\"true\"/><u5 isNull=\"true\"/><l><li>Li1</li><li>Li2</li><li>Li3</li></l><l2><li>S1</li><li>S2</li><li>S3</li></l2><l3><li>true</li><li>false</li><li>true</li></l3><l4><li><li>1</li><li>2</li><li>3</li></li><li><li>4</li><li>5</li><li>6</li></li><li><li>7</li><li>8</li><li>0</li></li></l4><lu isNull=\"true\"/><b><c>true</c></b></unionAndList>" >>, 
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
    [?_assertMatch(<< "<envelope><header><authTicket>MTIz</authTicket></header><body><getAuthTicket><login>test1</login><password>topsecret</password></getAuthTicket></body></envelope>" >>, 
      test_generate(envelope, 
	  {envelope, {header, <<"123">>},
          {getAuthTicket, "test1", "topsecret"}} )) 
    ].

fault_test_() ->
    [].
