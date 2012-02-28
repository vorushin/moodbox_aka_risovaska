-module(moodbox_xml_parser).

-export([parse/3, parse_file/3]).

%% for test reason only
-export([test_file/2, get_test_model/0]).

-include_lib("eunit/include/eunit.hrl").
-include("moodbox_model.hrl").

-define(TestDirPath, "priv/test/xml_parser").

-define(model, moodbox_model).
-define(datetime, moodbox_datetime).
-define(test_model_definition, moodbox_model_test_definition).

-record(state, {current_token, type_name, model, stack=[]}).
-record(token, {type, field, value}).

-define(empty_document_token, #token{type = document, field = undefined, value = undefined}).

%%% exceptions
-export([get_exception_message/1]).
-include("moodbox_exception.hrl").
-record(document_root_type_mismatch, {}).
-record(not_member_of_union, {type, union_type}).
-record(not_member_of_enum, {value, enum_name}).
-record(list_type_mismatch, {type, list_type}).
-record(required_field_not_present, {type_name, field_name}).
-record(required_value_not_present, {type_name, field_name, value_type}).
-record(unexpected_chars, {type, chars}).
-record(unhandled_sax_event, {event}).


parse_file(Model, DocumentRootTypeName, FileName) ->
    {ok, XmlBin} = file:read_file(FileName),
    parse(Model, DocumentRootTypeName, XmlBin).

parse(Model, DocumentRootTypeName, XmlBin) ->
    State = #state{current_token = #token{type = document}, type_name = DocumentRootTypeName, model = Model},
    {ok, Result, _Rest} = erlsom:parse_sax(XmlBin, State, fun handle_sax_event/2),
    Result.

handle_sax_event({processingInstruction, _Target, _Data}, State) ->
    State;
handle_sax_event(startDocument, #state{current_token = ?empty_document_token, stack = []} = State) ->
    State;
handle_sax_event({startElement, [] = _Uri, Element, [] = _Prefix, [] = _Attrs}, #state{current_token = ?empty_document_token = Token, stack = [], type_name = TypeName, model = Model} = State) ->
    ElementType = list_to_existing_atom(Element),
    if ElementType =/= TypeName ->
            ?error(#document_root_type_mismatch{});
       true ->
            Instance = ?model:create_type_instance(Model, ElementType),
            State#state{current_token = #token{type = ElementType, value = Instance}, stack = [Token]}
    end;
handle_sax_event({startElement, [] = _Uri, Element, [] = _Prefix, Attrs}, #state{current_token = #token{type = Type} = Token, stack = Stack, model = Model} = State) ->
    FieldOrTypeName = list_to_existing_atom(Element),
    case Type of
	#list{type = #union{types = UnionTypes}} -> % list of unions
	    FieldName = undefined,
	    TypeName = FieldOrTypeName,
	    case lists:member(TypeName, UnionTypes) of
	      true  ->
		    FieldType = TypeName;
	      false ->
		    FieldType = undefined,
		    ?error(#not_member_of_union{type = TypeName, union_type = Type#list.type})
	    end;
	#list{type = ListType} ->
	    FieldName = undefined,
	    TypeName = FieldOrTypeName,
	    if
		ListType == TypeName -> % list
		    FieldType = TypeName;
		TypeName == li -> % short list element notation: <li>...</li>
		    FieldType = Type#list.type;
		is_record(ListType, list) -> % list of lists
		    FieldType = ListType;
		true ->          
		    FieldType = undefined,
		    ?error(#list_type_mismatch{type = TypeName, list_type = ListType})
	    end;
	#union{types = UnionTypes} -> % union 
	    FieldName = undefined,
	    case {FieldOrTypeName, Attrs} of                                         
		{list, [{attribute, "type", [], [], ListType}]} -> % list in union: #union{types = [#list{type = int}]}
		    FieldType = #list{type = list_to_existing_atom(ListType)};
		_Other ->
		    FieldType = FieldOrTypeName		
	    end,
	    case lists:member(FieldType, UnionTypes) of
	      true -> 
		    ok;
	      false ->
		    ?error(#not_member_of_union{type = FieldType, union_type = Type})
	    end;
	_RegularType ->
	    FieldName = FieldOrTypeName,
	    #field{type = FieldType} = ?model:get_field_info(Model, Type, FieldName)
    end,
    case Attrs of
	[{attribute, "isNull", [], [], "true"}] ->
	    Instance = undefined;
	_ ->
	    Instance = ?model:create_type_instance(Model, FieldType)
    end,
    State#state{current_token = #token{type = FieldType, value = Instance}, stack = [Token#token{field = FieldName} | Stack]};
handle_sax_event({characters, Chars}, #state{current_token = #token{type = Type, field = undefined} = Token, model = Model} = State) ->
    State#state{current_token = fill_token(Token, parse_item(Model, Type, Chars))};
handle_sax_event({characters, Chars}, #state{current_token = #token{type = Type}} = _State) ->
    ?error(#unexpected_chars{type = Type, chars = Chars});
handle_sax_event({ignorableWhitespace, Chars}, #state{current_token = #token{type = string = Type, field = undefined} = Token, model = Model} = State) ->
    State#state{current_token = fill_token(Token, parse_item(Model, Type, Chars))};
handle_sax_event({ignorableWhitespace, _Chars}, State) ->
    State;
handle_sax_event({endElement, [], _Element, []}, #state{current_token = #token{type = Type, value = Value}, stack = [#token{type = StackTopType, field = StackTopFieldName, value = StackTopValue} = StackTopToken | StackRest], model = Model} = State) ->
    if
	is_record(Type, union) ->
	    ResultValue = ?model:update_field(Model, StackTopType, StackTopFieldName, StackTopValue, Value);
	is_tuple(Value) ->
	    #struct{name = Type, fields = FieldsInfo} = ?model:get_type_info(Model, Type),
	    case fill_defaults_and_check(Value, FieldsInfo) of
		{field_not_present, MissedFieldName} ->
		    ReadyValue = undefined,
		    ?error(#required_field_not_present{type_name = Type, field_name = MissedFieldName});
		{ok, ReadyValue} ->
		    ok
	    end,
	    ResultValue = ?model:update_field(Model, StackTopType, StackTopFieldName, StackTopValue, ReadyValue);
	is_list(Value) andalso is_record(Type, list) ->
	    ResultValue = ?model:update_field(Model, StackTopType, StackTopFieldName, StackTopValue, lists:reverse(Value));
	true ->
	    if
		Value == parser__not_parsed ->
		    ?error(#required_value_not_present{type_name = StackTopType, field_name = StackTopFieldName, value_type = Type});
		true ->
		    ok
	    end,
	    ResultValue = ?model:update_field(Model, StackTopType, StackTopFieldName, StackTopValue, Value)
    end,
    State#state{current_token = StackTopToken#token{value = ResultValue}, stack = StackRest};
handle_sax_event(endDocument, #state{current_token = #token{type = document, field = undefined, value = Result}, stack = []}) ->
    Result;
handle_sax_event(Event, _State) ->
    ?error(#unhandled_sax_event{event = Event}).
    
fill_token(Token, Value) ->
    Token#token{field = filled, value = Value}.

fill_defaults_and_check(Value, FieldsInfo) ->
    fill_defaults_and_check(2, Value, FieldsInfo).
fill_defaults_and_check(_Index, Value, [] = _FieldsInfo) ->
    {ok, Value};
fill_defaults_and_check(Index, Value, [FieldInfo | RestFieldsInfo] = _FieldsInfo) ->
    if
	element(Index, Value) == parser__not_parsed ->
	    #field{name = FieldName, is_optional = IsOptional, default_value = DefaultFieldValue} = FieldInfo,
	    if 
		IsOptional == true ->
		    ResultValue = setelement(Index, Value, DefaultFieldValue),
		    fill_defaults_and_check(Index + 1, ResultValue, RestFieldsInfo);
		true ->
		    {field_not_present, FieldName}
	    end;
	true ->
	    fill_defaults_and_check(Index + 1, Value, RestFieldsInfo)
    end.


%% internal functions

parse_item(_Model, string, Chars) ->
    Chars;
parse_item(_Model, int, Chars) ->
    list_to_integer(Chars);
parse_item(_Model, atom, Chars) ->
    list_to_existing_atom(Chars);
parse_item(_Model, float, Chars) ->
    list_to_float(Chars);
parse_item(_Model, bool, Chars) ->
    list_to_bool(Chars);
parse_item(_Model, datetime, Chars) ->
    ?datetime:from_xsd(Chars);
parse_item(_Model, binary, Chars) ->
    base64:decode(Chars);
parse_item(Model, EnumTypeName, Chars) ->
    Atom = list_to_existing_atom(Chars),
    #enum{values = Values} = ?model:get_type_info(Model, EnumTypeName),
    case lists:keymember(Atom, 1, Values) of
	true ->
	    Atom;
	false ->
	    ?error(#not_member_of_enum{value = Atom, enum_name = EnumTypeName})
    end.

list_to_bool(L) ->
    case list_to_existing_atom(L) of
	true -> 
	    true;
	false -> 
	    false
    end.


%%% exceptions handling

get_exception_message(?exception_data(#document_root_type_mismatch{})) ->
    io_lib:format("Document root type mismatch", []);
get_exception_message(?exception_data(#not_member_of_union{type = Type, union_type = UnionType})) ->
    io_lib:format("Type ~p is not a member of union ~p", [Type, UnionType]);
get_exception_message(?exception_data(#not_member_of_enum{value = Value, enum_name = EnumName})) ->
    io_lib:format("Value ~s is not a member of enum ~s", [Value, EnumName]);
get_exception_message(?exception_data(#list_type_mismatch{type = Type, list_type = ListType})) ->
    io_lib:format("Instance of type ~p cannot be a member of list of ~p", [Type, ListType]);
get_exception_message(?exception_data(#required_field_not_present{type_name = TypeName, field_name = FieldName})) ->
    io_lib:format("Field ~s of type ~s is required but not present", [FieldName, TypeName]);
get_exception_message(?exception_data(#required_value_not_present{type_name = TypeName, field_name = FieldName, value_type = ValueType})) ->
    io_lib:format("Field ~s of type ~s value representation is required but not present (value type is ~s)", [FieldName, TypeName, ValueType]);
get_exception_message(?exception_data(#unexpected_chars{type = Type, chars = Chars})) ->
    io_lib:format("Unexpected chars ~p inside of type ~p", [Chars, Type]);
get_exception_message(?exception_data(#unhandled_sax_event{event = Event})) ->
    io_lib:format("Unhandled SAX event: ~p", [Event]).


%% test functions

get_test_model() ->
    ?model:create(?test_model_definition:get_definition()).

test_file(FileName, DocumentRootTypeName) ->
    parse_file(get_test_model(), DocumentRootTypeName, filename:join(?TestDirPath, FileName)).

simple_types_test_() ->
    [?_assertMatch(
	{simpletypes, atom1, 11, 1.1, <<"123">>, true, "   \n  \t   ",
             1217000002000000, atom3, {b,"some string"}, "empty",undefined}, 
	test_file("test_simple_types.xml", simpletypes))].

envelope_test_() ->
    [?_assertMatch(
	{envelope,{header,<<"123">>},
          {getAuthTicket,"test1","topsecret"}},
	test_file("test_envelope.xml", envelope))].

union_and_list_test_() ->
    [?_assertMatch(
	{unionAndList,{b,"7"},
              "union text",1.3,
              [1.3,1.5,1.7],
              ["text1","text2","text3"],
              ["Li1","Li2","Li3"],
              ["S1","S2","S3"],
              [true,false,true],
              [[1,2,3],[4,5,6],[7,8,0]],
              ["SomeText",1185355875000000,11,{b,"9"}],
              {b,"true"}}, 
    test_file("test_union_and_list.xml", unionAndList))].

all_test_() ->
    [?_assertMatch(
	{all,{b,"7"},
     "union text",1.3,
     [1.3,1.5,1.7],
     ["text1","text2","text3"],
     ["Li1","Li2","Li3"],
     ["S1","S2","S3"],
     [true,false,true],
     ["SomeText",1185355875000000,11,{b,"9"}],
     atom1,11,1.1,<<"123">>,true,"test text",1217000002000000,
     atom3,
     {b,"some string"},
     "empty",undefined}, 
    test_file("test_all.xml", all))].

fault_test_() ->
    [?_assertException(error, function_clause, 
		       test_file("test_wrong_binary.xml", simpletypes)),
     ?_assertException(error, {case_clause, boolean}, % @todo provide correct exception for this case
		       test_file("test_wrong_boolean.xml", simpletypes)),
     ?_assertException(throw, ?exception_data(#not_member_of_union{type = bool, union_type = #union{types = [b, int, string]}}), 
		       test_file("test_wrong_union_type.xml", unionAndList)),
     ?_assertException(throw, ?exception_data(#not_member_of_union{type = #list{type = string}, union_type = #union{types = [int, #list{type = float}]}}), 
		       test_file("test_wrong_union_list_type.xml", unionAndList)),
     ?_assertException(throw, ?exception_data(#not_member_of_union{type = float, union_type = #union{types = [b, int, string, datetime]}}), 
		       test_file("test_wrong_list_union_type.xml", unionAndList)),
     ?_assertException(throw, ?exception_data(#list_type_mismatch{type = int, list_type = string}), 
		       test_file("test_wrong_list_type.xml", unionAndList)),
     ?_assertException(throw, ?exception_data(#unexpected_chars{type = envelope, chars = "\r\n  body>\r\n  \t"}), 
		       test_file("test_wrong_xml.xml", envelope)),
     ?_assertException(throw, {error, "Malformed: Tags don't match"}, 
		       test_file("test_wrong_xml_2.xml", envelope)),
     ?_assertException(throw, ?exception_data(#not_member_of_union{type = getAge, union_type = #union{types = [getAuthTicket, getProfile]}}), 
		       test_file("test_wrong_envelope_function.xml", envelope)),
     ?_assertException(throw, ?exception_data(#required_field_not_present{type_name = simpletypes, field_name = i}), 
		       test_file("test_wrong_obligatory_field.xml", simpletypes)),
     ?_assertException(error, {badmatch, "2008-05T15:33:22.000"}, % @todo provide correct exception for this case
		       test_file("test_wrong_datetime.xml", simpletypes)),
     ?_assertException(throw, ?exception_data(#not_member_of_enum{value = atom7, enum_name = e}), 
		       test_file("test_wrong_enum_value.xml", simpletypes)),
     ?_assertException(error, badarg, 
		       test_file("test_wrong_integer.xml", simpletypes)) % @todo provide correct exception for this case
    ].
