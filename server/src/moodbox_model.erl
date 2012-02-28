-module(moodbox_model).

-export([create/1, create_record_file/2, create_type_instance/2, get_types_info/1, set_types_info/2, get_type_info/2, 
	 get_type_info_silently/2, is_primitive/1, is_enum/2, get_result_type/1, get_field_info/3, update_field/5]).
-export([is_auth_free/2]).
-export([to_pascal_case/1, to_camel_case/1]).
-export([define_struct/4, define_struct/3, define_struct/2, define_fault/2, define_fault/3, define_function/5, define_function/4, define_function/3, 
	 define_field/4, define_field/3, define_field/2, define_field_default/5, define_field_default/4, define_field_default/3, define_field_optional/4, 
	 define_field_optional/3, define_field_optional/2, define_enum/3, define_enum/4, define_enum/2]).
-export([create_for_cpp_generator/1, get_type_info_for_cpp_generator/2, get_type_info_silently_for_cpp_generator/2, is_enum_for_cpp_generator/2]).

-include_lib("eunit/include/eunit.hrl").
-include("moodbox_model.hrl").

%%% exceptions
-export([get_exception_message/1]).
-include("moodbox_exception.hrl").
-record(type_not_found, {type_name}).
-record(field_not_found, {type_name, field_name}).


create(ModelDefinition) ->
    set_types_info(ModelDefinition, create_types_info(get_types_info(ModelDefinition), dict:new())).

create_types_info([] = _TypesInfo, NewTypesInfo) ->
    NewTypesInfo;
create_types_info([TypeInfo | Rest] = _TypesInfo, NewTypesInfo) ->
    case TypeInfo of
	#struct{name = Name, id = Id, return_type = ReturnType} when ReturnType =/= undefined ->
	    NewName = list_to_atom(atom_to_list(Name) ++ "_result"),
	    NewTypeInfo = #struct{id = case Id of undefined -> undefined; _ -> Id + 1 end, name = NewName, fields = [?Field(result, ReturnType)], is_result_for = Name},
	    create_types_info(Rest, dict:append(NewName, NewTypeInfo, dict:append(Name, TypeInfo, NewTypesInfo)));
	#struct{name = Name} ->
	    create_types_info(Rest, dict:append(Name, TypeInfo, NewTypesInfo));
	#field{name = Name} ->
	    create_types_info(Rest, dict:append(Name, TypeInfo, NewTypesInfo));
	#enum{name = Name} ->
	    create_types_info(Rest, dict:append(Name, TypeInfo, NewTypesInfo));
	_Other ->
	    create_types_info(Rest, NewTypesInfo)
    end.

create_record_file(Model, FileName)->
    case filelib:is_file(FileName) of
      true ->
	  ok = file:delete(FileName);
      false ->
	  ok
    end,
    FieldOutputFunction = fun(#field{name = Name, default_value = DefaultValue} = _FieldInfo, {IoDevice, Separator}) ->
		    io:format(IoDevice, "~s~s", [Separator, Name]),
		    if
			DefaultValue =/= undefined ->
			    io:format(IoDevice, " = ~p", [DefaultValue]);
			true ->
			    ok
		    end,		    
		    {IoDevice, ", "} % new separator
	end,
    {ok, IoDevice} = file:open(FileName, [write, append]),
    TypeOutputFunction = fun(TypeInfo) -> 
		    case TypeInfo of 
		      #struct{name = Name, fields = Fields} -> 
			    io:format(IoDevice, "-record(~p, {", [Name]),
			    foreach_stated(FieldOutputFunction, Fields, {IoDevice, ""}),
			    io:format(IoDevice, "}).~n", []);
		      _Other -> 
			    ok
			    
		    end
	end,
    lists:foreach(TypeOutputFunction, get_types_info(Model)),
    file:close(IoDevice).
    
foreach_stated(_Fun, [], _State) ->
	ok;
foreach_stated(Fun, [Head | Tail], State) ->
	NewState = Fun(Head, State),
	foreach_stated(Fun, Tail, NewState).

create_type_instance(_Model, string) ->
    "";
create_type_instance(_Model, int) ->
    parser__not_parsed;
create_type_instance(_Model, atom) ->
    parser__not_parsed;
create_type_instance(_Model, float) ->
    parser__not_parsed;
create_type_instance(_Model, bool) ->
    parser__not_parsed;
create_type_instance(_Model, datetime) ->
    parser__not_parsed;
create_type_instance(_Model, binary) ->
    << >>;
create_type_instance(_Model, Record) when is_record(Record, list) ->
    [];
create_type_instance(_Model, Record) when is_record(Record, union) ->
    parser__not_parsed;
create_type_instance(Model, TypeName) ->
    case get_type_info(Model, TypeName) of
	Record when is_record(Record, struct) ->
	    Arity = length(Record#struct.fields) + 1,
	    Result = setelement(1, erlang:make_tuple(Arity, parser__not_parsed), Record#struct.name),
	    Result;
	Record when is_record(Record, enum) ->
	    parser__not_parsed
    end.

get_types_info(Model) ->
    Model#model.types_info.
set_types_info(Model, TypesInfo) ->
    Model#model{types_info = TypesInfo}.

get_type_info(Model, TypeName) ->
    case dict:fetch(TypeName, Model#model.types_info) of
	[Value] -> % it was deleted by optimization: when is_record(Value, struct) orelse is_record(Value, enum)
	    Value
    end.

get_type_info_silently(Model, TypeName) ->
    case dict:find(TypeName, Model#model.types_info) of
	{ok, [Value]} ->
	    Value;
	error ->
	    undefined
    end.

is_primitive(string) ->
    true;
is_primitive(int) ->
    true;
is_primitive(atom) ->
    true;
is_primitive(float) ->
    true;
is_primitive(bool) ->
    true;
is_primitive(datetime) ->
    true;
is_primitive(binary) ->
    true;
is_primitive(#list{}) ->
    true;
is_primitive(#union{}) ->
    true;
is_primitive(_Other) ->
    false.

is_enum(Model, Type) when is_atom(Type) ->
    is_record(get_type_info_silently(Model, Type), enum);
is_enum(_Model, _Type) ->
    false.

get_result_type(FuncName) ->
    list_to_existing_atom(atom_to_list(FuncName) ++ "_result").

get_field_info(Model, TypeName, FieldName) ->
    [#struct{fields = FieldsInfo}] = dict:fetch(TypeName, Model#model.types_info),
    case get_field_info(FieldsInfo, FieldName) of
	{error, not_found} ->
	    ?error(#field_not_found{type_name = TypeName, field_name = FieldName});
	{ok, Result} ->
	    Result
    end.

get_field_info([] = _FieldsInfo, _FieldName) ->
    {error, not_found};
get_field_info([#field{name = FieldName} = FieldInfo | _Rest] = _FieldsInfo, FieldName) ->
    {ok, FieldInfo};
get_field_info([_FieldInfo | Rest] = _FieldsInfo, FieldName) ->
    get_field_info(Rest, FieldName).

get_field_list_index([] = _FieldsInfo, _FieldName, _Index) ->
    {error, not_found};
get_field_list_index([#field{name = FieldName} = _FieldInfo | _Rest] = _FieldsInfo, FieldName, Index) ->
    {ok, Index};
get_field_list_index([_FieldInfo | Rest] = _FieldsInfo, FieldName, Index) ->
    get_field_list_index(Rest, FieldName, Index + 1).

update_field(_Model, document = _TypeName, undefined = _FieldName, undefined = _Instance, Value) ->
    Value;
update_field(Model, Type, FieldName, Instance, Value) ->
    if
	is_record(Type, list) ->
	    [Value | Instance];
	is_record(Type, union) ->
	    Value;
	true ->
	    [#struct{fields = FieldsInfo}] = dict:fetch(Type, Model#model.types_info),
	    case get_field_list_index(FieldsInfo, FieldName, 1) of
		{error, not_found} ->
		    ?error(#field_not_found{type_name = Type, field_name = FieldName});
		{ok, Index} ->
		    setelement(Index + 1, Instance, Value)
	    end	    
    end.


%%% info functions

is_auth_free(#model{auth_free_functions = AuthFreeFuncs} = _Model, Func) ->
    if
	AuthFreeFuncs == undefined ->
	    false;
	true ->
	    lists:member(Func, AuthFreeFuncs)
    end.

                
%%% tool functions

to_pascal_case(Chars) ->
    to_pascal_case(Chars, upper, "").
to_pascal_case([], _State, Result) ->
    lists:reverse(Result);
to_pascal_case([$_ | Chars], _State, Result) ->
    to_pascal_case(Chars, upper, Result);
to_pascal_case([Char | Chars], upper, Result) ->
    to_pascal_case(Chars, lower, [string:to_upper(Char) | Result]);
to_pascal_case([Char | Chars], lower, Result) ->
    to_pascal_case(Chars, lower, [Char | Result]).

to_camel_case(Chars) ->
    to_pascal_case(Chars, lower, "").


%%% Cpp specific functions

create_for_cpp_generator(ModelDefinition) ->
    set_types_info(ModelDefinition, create_types_info_for_cpp_generator(get_types_info(ModelDefinition), [])).

create_types_info_for_cpp_generator([] = _TypesInfo, NewTypesInfo) ->
    lists:reverse(NewTypesInfo);
create_types_info_for_cpp_generator([TypeInfo | Rest] = _TypesInfo, NewTypesInfo) ->
    case TypeInfo of
	#struct{name = Name, id = Id, return_type = ReturnType} when ReturnType =/= undefined ->
	    NewTypeInfo = #struct{id = Id + 1, name = list_to_atom(atom_to_list(Name) ++ "_result"), 
				  fields = [?Field(result, ReturnType)], is_result_for = Name},
	    create_types_info_for_cpp_generator(Rest, [TypeInfo, NewTypeInfo | NewTypesInfo]);
	_Other ->
	    create_types_info_for_cpp_generator(Rest, [TypeInfo | NewTypesInfo])
    end.

get_type_info_for_cpp_generator(Model, TypeName) ->
    get_type_info(get_types_info(Model), TypeName, true).
get_type_info_silently_for_cpp_generator(Model, TypeName) ->
    get_type_info(get_types_info(Model), TypeName, false).

get_type_info([] = _TypesInfo, TypeName, ErrorIfNotFound) ->
    case ErrorIfNotFound of
	true ->
	    ?error(#type_not_found{type_name = TypeName});
	false ->
	    undefined
    end;
get_type_info([#struct{name = TypeName} = TypeInfo | _Rest] = _TypesInfo, TypeName, _ErrorIfNotFound) ->
    TypeInfo;
get_type_info([#enum{name = TypeName} = TypeInfo | _Rest] = _TypesInfo, TypeName, _ErrorIfNotFound) ->
    TypeInfo;
get_type_info([_TypeInfo | Rest] = _TypesInfo, TypeName, ErrorIfNotFound) ->
    get_type_info(Rest, TypeName, ErrorIfNotFound).

is_enum_for_cpp_generator(Model, Type) when is_atom(Type) ->
    is_record(get_type_info_silently_for_cpp_generator(Model, Type), enum);
is_enum_for_cpp_generator(_Model, _Type) ->
    false.

%%% definition helpers

define_struct(Id, Name, Fields, CppOptions) ->
    #struct{id = Id, name = Name, fields = Fields, cpp_options = CppOptions}.
define_struct(Id, Name, Fields) ->
    define_struct(Id, Name, Fields, undefined).
define_struct(Name, Fields) ->
    define_struct(undefined, Name, Fields, undefined).
define_fault(Id, Name, Fields) ->
    #struct{id = Id, name = Name, fields = Fields, is_fault = true}.
define_fault(Name, Fields) ->
    define_fault(undefined, Name, Fields).
define_function(Id, Name, Fields, ReturnType, CppOptions) ->
    #struct{id = Id, name = Name, fields = Fields, return_type = ReturnType, cpp_options = CppOptions}.
define_function(Id, Name, Fields, ReturnType) ->
    define_function(Id, Name, Fields, ReturnType, undefined).
define_function(Name, Fields, ReturnType) ->
    define_function(undefined, Name, Fields, ReturnType, undefined).
define_field(Id, Name, Type, CppOptions) ->
    #field{id = Id, name = Name, type = Type, cpp_options = CppOptions}.
define_field(Id, Name, Type) ->
    define_field(Id, Name, Type, undefined).
define_field(Name, Type) ->
    define_field(1, Name, Type, undefined).
define_field_default(Id, Name, Type, DefaultValue, CppOptions) ->
    #field{id = Id, name = Name, type = Type, is_optional = true, default_value = DefaultValue, cpp_options = CppOptions}.
define_field_default(Id, Name, Type, DefaultValue) ->
    define_field_default(Id, Name, Type, DefaultValue, undefined).
define_field_default(Name, Type, DefaultValue) ->
    define_field_default(1, Name, Type, DefaultValue, undefined).
define_field_optional(Id, Name, Type, CppOptions) ->
    #field{id = Id, name = Name, type = Type, is_optional = true, cpp_options = CppOptions}.
define_field_optional(Id, Name, Type) ->
    define_field_optional(Id, Name, Type, undefined).
define_field_optional(Name, Type) ->
    define_field_optional(1, Name, Type, undefined).
define_enum(Id, Name, Values) ->
    #enum{id = Id, name = Name, values = Values}.
define_enum(Id, Name, Values, CppOptions) ->
    #enum{id = Id, name = Name, values = Values, cpp_options = CppOptions}.
define_enum(Name, Values) ->
    define_enum(undefined, Name, Values).


%%% exceptions handling

get_exception_message(?exception_data(#type_not_found{type_name = TypeName})) ->
    io_lib:format("Type ~s could not be found in the Model", [TypeName]);
get_exception_message(?exception_data(#field_not_found{type_name = TypeName, field_name = FieldName})) ->
    io_lib:format("Field ~s could not be found in the Type ~s", [FieldName, TypeName]).
