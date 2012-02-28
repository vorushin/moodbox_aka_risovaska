-module(moodbox_cpp_codegen).

-export([generate/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-include("moodbox_model.hrl").

-define(OutPath, "priv/generated_cpp").

-define(NamespaceName, "MoodBox").
-define(TopClass, transportable_object).

-define(ModelClassName, "MoodBoxModel").
-define(ModelBaseClassName, "Model").

-define(ServerProxyClassName, "MoodBoxServer").
-define(ServerProxyBaseClassName, "ServerProxyBase").

-define(FaultType, fault).

-define(ListWrappersFileName, "listwrapperobjects").
-define(ListWrappersBaseClassName, "ListOfSimpleWrapperObject").
-define(ListWrappersClassNameFormat, "ListOf~sWrapperObject").

-define(model, moodbox_model).


generate(FullModel) ->
    remove_files(),
    Model = remove_types_to_skip(FullModel),
    generate_model(Model),
    generate_list_wrappers(Model),
    generate_server_proxy(Model),
    generate_types(Model, ?model:get_types_info(Model)).

remove_types_to_skip(Model) ->
    ?model:set_types_info(Model, remove_types_to_skip(Model, ?model:get_types_info(Model), [])).
remove_types_to_skip(_FullModel, [] = _TypesInfo, NewTypesInfo) ->
    lists:reverse(NewTypesInfo);
remove_types_to_skip(FullModel, [TypeInfo | Rest] = _TypesInfo, NewTypesInfo) ->
    case is_type_to_skip(FullModel, TypeInfo) of
	true ->
	    remove_types_to_skip(FullModel, Rest, NewTypesInfo);
	false ->
	    remove_types_to_skip(FullModel, Rest, [remove_types_to_skip_from_unions(FullModel, TypeInfo) | NewTypesInfo])
    end.
remove_types_to_skip_from_unions(FullModel, #struct{fields = Fields, return_type = ReturnType} = TypeInfo) ->
    TypeInfo#struct{fields = remove_types_to_skip_from_union_fields(FullModel, Fields, []), return_type = remove_types_to_skip_from_union(FullModel, ReturnType)};
remove_types_to_skip_from_unions(_FullModel, TypeInfo) ->
    TypeInfo.
remove_types_to_skip_from_union_fields(_FullModel, [] = _Fields, NewFields) ->
    lists:reverse(NewFields);
remove_types_to_skip_from_union_fields(FullModel, [#field{type = Type} = FieldInfo | Rest] = _Fields, NewFields) ->
    remove_types_to_skip_from_union_fields(FullModel, Rest, [FieldInfo#field{type = remove_types_to_skip_from_union(FullModel, Type)} | NewFields]).
remove_types_to_skip_from_union(FullModel, #union{types = Types} = Type) ->
    Type#union{types = remove_types_to_skip_from_union(FullModel, Types, [])};
remove_types_to_skip_from_union(FullModel, #list{type = ListType} = Type) ->
    Type#list{type = remove_types_to_skip_from_union(FullModel, ListType)};
remove_types_to_skip_from_union(_FullModel, Type) ->
    Type.
remove_types_to_skip_from_union(_FullModel, [] = _Types, NewTypes) ->
    lists:reverse(NewTypes);
remove_types_to_skip_from_union(FullModel, [Type | Rest] = _Types, NewTypes) ->
    case is_type_to_skip(FullModel, ?model:get_type_info_silently_for_cpp_generator(FullModel, Type)) of
	true ->
	    remove_types_to_skip_from_union(FullModel, Rest, NewTypes);
	false ->
	    remove_types_to_skip_from_union(FullModel, Rest, [remove_types_to_skip_from_union(FullModel, Type) | NewTypes])
    end.
is_type_to_skip(_FullModel, #struct{cpp_options = #cpp_class_options{skip = true}} = _TypeInfo) ->
    true;
is_type_to_skip(FullModel, #struct{is_result_for = CallType} = _TypeInfo) when CallType =/= undefined ->
    is_type_to_skip(FullModel, ?model:get_type_info_for_cpp_generator(FullModel, CallType));
is_type_to_skip(_FullModel, _TypeInfo) ->
    false.

remove_files() ->
    {ok, Files} = file:list_dir(?OutPath),
    remove_files(Files).
remove_files([] = _FileNames) ->
    ok;
remove_files([FileName | Rest] = _FileNames) ->
    Path = filename:join(?OutPath, FileName),
    case file:read_file_info(Path) of
	{ok, #file_info{type = regular}} ->
	    ok = file:delete(Path);
	{ok, #file_info{}} ->
	    ok
    end,
    remove_files(Rest).

generate_model(Model) ->
    generate_model_h(Model),
    generate_model_cpp(Model).

generate_server_proxy(Model) ->
    generate_server_proxy_h(Model),
    generate_server_proxy_cpp(Model).

generate_types(_Model, []) ->
    ok;
generate_types(Model, [TypeInfo | Rest]) ->
    generate_h(Model, TypeInfo),
    generate_cpp(Model, TypeInfo),
    generate_types(Model, Rest).

generate_model_h(_Model) ->
    {ok, IoDevice} = file:open(filename:join(?OutPath, get_file_name(?ModelClassName, "h")), [write]),
    try
	NamespaceName = ?NamespaceName,
	BaseClassName = ?ModelBaseClassName,
	ClassName = ?ModelClassName,
	DefineSymbol = get_define_symbol(?ModelClassName),    
	io:format(IoDevice,
		  "#ifndef ~s_H~n"
		  "#define ~s_H~n~n", [DefineSymbol, DefineSymbol]),
	io:format(IoDevice,
		  "~n"
		  "#include \"model.h\"~n~n"
		  "namespace ~s~n"
		  "{~n~n"
		  "class ~s : public ~s~n"
		  "{~n"
		  "public:~n"
		  "    ~s();~n"
		  "    virtual ~~~s();~n~n", [NamespaceName, ClassName, BaseClassName, ClassName, ClassName]),
	io:format(IoDevice,
		  "    virtual void fill();~n"
		  "};~n~n"
		  "}~n~n"
		  "#endif // ~s_H", [DefineSymbol])
    after
	file:close(IoDevice)
    end.

generate_model_cpp(Model) ->
    {ok, IoDevice} = file:open(filename:join(?OutPath, get_file_name(?ModelClassName, "cpp")), [write]),
    try
	NamespaceName = ?NamespaceName,
	BaseClassName = ?ModelBaseClassName,
	ClassName = ?ModelClassName,
	io:format(IoDevice,
		  "#include \"~s\"~n~n"
		  "namespace ~s~n"
		  "{~n~n"
		  "~s::~s() : ~s()~n"
		  "{~n", [get_file_name(ClassName, "h"), NamespaceName, ClassName, ClassName, BaseClassName]),
	io:format(IoDevice,
		  "}~n~n"
		  "~s::~~~s()~n"
		  "{~n", [ClassName, ClassName]),
	io:format(IoDevice,
		  "}~n~n", []),
	io:format(IoDevice,
		  "void ~s::fill()~n"
		  "{~n", [ClassName]),
	write_model_fill_instructions_types(IoDevice, ?model:get_types_info(Model)),
	io:nl(IoDevice),
	write_model_fill_instructions_properties(IoDevice, Model, ?model:get_types_info(Model)),
	io:format(IoDevice,
		  "}~n"
		  "~n"
		  "}~n", [])
    after
	file:close(IoDevice)
    end.    


generate_list_wrappers(Model) ->
    {ok, IoDevice} = file:open(filename:join(?OutPath, ?ListWrappersFileName ++ ".h"), [write]),
    try
	NamespaceName = ?NamespaceName,
	BaseClassName = ?ListWrappersBaseClassName,
	DefineSymbol = get_define_symbol(?ListWrappersFileName),
	ReadTypes = get_read_types(),
	io:format(IoDevice,
		  "#ifndef ~s_H~n"
		  "#define ~s_H~n~n", [DefineSymbol, DefineSymbol]),
	write_required_types(IoDevice, Model, dict:new(), ReadTypes),
	io:format(IoDevice,
		  "~n"
		  "#include \"transportableobject.h\"~n~n"
		  "namespace ~s~n"
		  "{~n~n", [NamespaceName]),
	write_list_wrappers(IoDevice, Model, BaseClassName, ReadTypes),
	io:format(IoDevice,
		  "}~n~n"
		  "#endif // ~s_H", [DefineSymbol])
    after
	file:close(IoDevice)
    end.
    

generate_server_proxy_h(Model) ->
    {ok, IoDevice} = file:open(filename:join(?OutPath, get_file_name(?ServerProxyClassName, "h")), [write]),
    try
	NamespaceName = ?NamespaceName,
	BaseClassName = ?ServerProxyBaseClassName,
	ClassName = ?ServerProxyClassName,
	DefineSymbol = get_define_symbol(?ServerProxyClassName),    
	io:format(IoDevice,
		  "#ifndef ~s_H~n"
		  "#define ~s_H~n~n", [DefineSymbol, DefineSymbol]),

	write_required_types(IoDevice, Model, get_required_types_for_methods(Model), [?TopClass, ?FaultType]),

	io:format(IoDevice,
		  "~n"
		  "#include \"~s\"~n~n"
		  "namespace ~s~n"
		  "{~n~n"
		  "class ~s : public ~s~n"
		  "{~n"
		  "public:~n"
		  "    ~s(Model* model, TransportChannelBase* channel);~n~n"
		  "    virtual void resultFaultCall(Callback callback, QVariant state, ~s fault, qint32 resultTypeId);~n~n", [get_file_name(BaseClassName, "h"), NamespaceName, ClassName, BaseClassName, ClassName, get_type_definition(Model, ?FaultType)]),
	write_method_defs(IoDevice, Model, ?model:get_types_info(Model)),
	io:format(IoDevice,
		  "};~n~n"
		  "}~n~n"
		  "#endif // ~s_H", [DefineSymbol])
    after
	file:close(IoDevice)
    end.

generate_server_proxy_cpp(Model) ->
    {ok, IoDevice} = file:open(filename:join(?OutPath, get_file_name(?ServerProxyClassName, "cpp")), [write]),
    try
	NamespaceName = ?NamespaceName,
	BaseClassName = ?ServerProxyBaseClassName,
	ClassName = ?ServerProxyClassName,

	io:format(IoDevice,
		  "#include \"~s\"~n~n", [get_file_name(ClassName, "h")]),

	write_required_types(IoDevice, Model, get_required_types_for_results(Model), []),

	io:format(IoDevice,
		  "~n"
		  "namespace ~s~n"
		  "{~n~n"
		  "~s::~s(Model* model, TransportChannelBase* channel) : ~s(model, channel)~n"
		  "{~n", [NamespaceName, ClassName, ClassName, BaseClassName]),
	io:format(IoDevice,
		  "}~n~n"
		  "void ~s::resultFaultCall(Callback callback, QVariant state, ~s fault, qint32 resultTypeId)~n"
		  "{~n", [ClassName, get_type_definition(Model, ?FaultType)]),

	case get_result_types_info(Model) of
	    [] ->
		io:format(IoDevice,
			  "    Q_UNUSED(callback);~n"
			  "    Q_UNUSED(resultTypeId);~n~n"
			  "    throw \"Unknown resultTypeId\";~n", []);
	    ResultTypesInfo ->
		io:format(IoDevice,		
			  "    switch(resultTypeId)~n"
			  "    {~n", []),
		write_result_fault_cases(IoDevice, ResultTypesInfo),
		io:format(IoDevice,
			  "        default:~n"
			  "            throw \"Unknown resultTypeId\";~n"
			  "    }~n", [])
	end,

	io:format(IoDevice,
		  "}~n~n", []),
	write_methods(IoDevice, Model, ClassName, ?model:get_types_info(Model)),
	io:format(IoDevice,
		  "~n"
		  "}~n", [])
    after
	file:close(IoDevice)
    end.    


generate_h(Model, #struct{name = TypeName, fields = FieldsInfo, is_result_for = CallType, is_fault = IsFault, cpp_options = CppOptions} = TypeInfo) ->
    MakeIntermediate = get_cpp_make_intermediate(CppOptions),
    GeneratedClassName = get_class_name(TypeName, MakeIntermediate),

    {ok, IoDevice} = file:open(filename:join(?OutPath, get_file_name(GeneratedClassName, "h")), [write]),
    try
	NamespaceName = ?NamespaceName,
	TopClassName = get_class_name(?TopClass),

	BaseClassName = TopClassName,
	ClassName = get_class_name(TypeName),
	GeneratedDataClassName = ClassName ++ "Data",
	DataClassName = ClassName ++ "Data",

	DefineSymbol = get_define_symbol(GeneratedClassName),
	IsSharedType = is_shared_type(Model, TypeInfo),
	IsNullable = is_nullable(Model, TypeInfo),
	IsResult = (CallType =/= undefined),
	io:format(IoDevice,
		  "#ifndef ~s_H~n"
		  "#define ~s_H~n~n", [DefineSymbol, DefineSymbol]),

	if
	    IsResult ->
		io:format(IoDevice,
			  "#include <QObject>~n", []);
	    true ->
		ok
	end,

	if
	    IsSharedType ->
		io:format(IoDevice,
			  "#include <QSharedData>~n", []);
	    true ->
		ok
	end,

	write_required_types(IoDevice, Model, get_required_types(Model, FieldsInfo), [?TopClass | if IsResult -> [?FaultType]; true -> [] end]),

	if
	    IsResult ->
		io:format(IoDevice,
			  "#include \"callbackcallertools.h\"~n", []);
	    true ->
		ok
	end,
	if
	    IsFault ->
		io:format(IoDevice,
			  "#include \"~s\"~n", [get_file_name(?ServerProxyBaseClassName, "h")]);
	    true ->
		ok
	end,

	io:format(IoDevice,
		  "~n"
		  "namespace ~s~n"
		  "{~n~n", [NamespaceName]),

	if
	    IsSharedType ->
		io:format(IoDevice,
			  "class ~s : public QSharedData~n"
			  "{~n"
			  "public:~n"
			  "    ~s();~n", [GeneratedDataClassName, GeneratedDataClassName]),
		case FieldsInfo of
		    [] ->
			ok;
		    _ ->
			io:format(IoDevice,
				  "    ~s(", [GeneratedDataClassName]),
			write_field_ctor_definitions(IoDevice, Model, FieldsInfo),
			io:format(IoDevice,
				  ");~n", [])
		end,
		io:format(IoDevice,
			  "    virtual ~~~s();~n~n", [GeneratedDataClassName]),
		write_fields(IoDevice, Model, FieldsInfo),
		io:format(IoDevice,
			  "};~n~n", []);
	    true ->
		ok
	end,

	io:format(IoDevice,
		  "class ~s : public ~s~n"
		  "{~n", [GeneratedClassName, BaseClassName]),
	if
	    MakeIntermediate ->
		io:format(IoDevice,
			  "protected:~n", []);
	    true ->
		io:format(IoDevice,
			  "public:~n", [])
	end,
	io:format(IoDevice,
		  "    ~s();~n", [GeneratedClassName]),
	case FieldsInfo of
	    [] ->
		ok;
	    _ ->
		io:format(IoDevice,
			  "    ~s(", [GeneratedClassName]),
		write_field_ctor_definitions(IoDevice, Model, FieldsInfo),
		io:format(IoDevice,
			  ");~n", [])
	end,
	if
	    MakeIntermediate ->
		io:format(IoDevice,
			  "public:~n", []);
	    true ->
		ok
	end,
	io:format(IoDevice,
		  "    virtual ~~~s();~n~n", [GeneratedClassName]),

	if
	    IsSharedType ->
		if 
		    IsNullable ->
			io:format(IoDevice,
				  "protected:~n"
				  "    ~s(~s* dataRef)~n"
				  "    {~n"
				  "        this->d = dataRef;~n"
				  "    }~n"
				  "public:~n", [GeneratedClassName, DataClassName]),
			if
			    MakeIntermediate ->
				ok;
			    true ->
				write_new_empty_shared_nullable(IoDevice, GeneratedClassName, DataClassName)
			end,
			io:format(IoDevice,
				  "    virtual bool isNull() const~n"
				  "    {~n"
				  "        return !d;~n"
				  "    }~n~n", []);
		    not(MakeIntermediate) ->
			write_new_empty(IoDevice, GeneratedClassName, true);
		    true ->
			ok
		end;
	    not(MakeIntermediate) ->
		write_new_empty(IoDevice, GeneratedClassName, false);
	    true ->
		ok
	end,

	if
	    IsResult ->
		io:format(IoDevice,
			  "    virtual void resultCall(Callback callback, QVariant state);~n~n", []);
	    true ->
		ok
	end,

	if
	    IsFault ->
		io:format(IoDevice,
			  "    virtual bool isFault() const~n"
			  "    {~n"
			  "        return true;~n"
			  "    }~n~n"
			  "    virtual void resultFaultCall(~s* server, Callback callback, QVariant state, qint32 resultTypeId);~n~n", [?ServerProxyBaseClassName]);
	    true ->
		ok
	end,

	write_get_set_defs(IoDevice, Model, FieldsInfo),
	io:format(IoDevice,
		  "~n"
		  "    static qint32 getRepresentedTypeId();~n~n"
		  "    virtual qint32 getTypeId() const;~n"
		  "    virtual void writeProperties(PropertyWriter *writer);~n"
		  "    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);~n~n"
		  "private:~n", []),

	if
	    IsSharedType ->
		case is_explicitly_shared(Model, TypeInfo) of
		    true ->
			io:format(IoDevice,
				  "    QExplicitlySharedDataPointer<~s> d;~n", [DataClassName]);
		    false ->
			io:format(IoDevice,
				  "    QSharedDataPointer<~s> d;~n", [DataClassName])
		end;
	    true ->
		write_fields(IoDevice, Model, FieldsInfo)
	end,

	io:format(IoDevice,
		  "};~n~n", []),


	if
	    IsResult ->
	    	ResultTypeInfo = ?model:get_type_info_for_cpp_generator(Model, CallType),
	    	ResultType = get_result_type(ResultTypeInfo),
		TypeDef = get_type_definition(Model, ResultType),
		FaultTypeDef = get_type_definition(Model, ?FaultType),
		ResultDefault = get_default_extended(Model, ResultType, undefined),
		FaultDefault = get_default_extended(Model, ?FaultType, undefined),
		io:format(IoDevice,
			  "class ~sCallbackCaller : public QObject~n"
			  "{~n"
			  "    Q_OBJECT~n~n"
			  "public:~n"
			  "    static void call(Callback &callback, QVariant state, ~s result)~n"
			  "    {~n"
			  "       ~sCallbackCaller caller;~n"
			  "       caller.callInternal(callback, state, ~s, result);~n"
			  "    }~n"
			  "    static void call(Callback &callback, QVariant state, ~s fault)~n"
			  "    {~n"
			  "        ~sCallbackCaller caller;~n"
			  "        caller.callInternal(callback, state, fault, ~s);~n"
			  "    }~n~n"
			  "signals:~n"
			  "    void callbackSignal(QVariant state, ~s fault, ~s result);~n~n"
			  "private:~n"
			  "    void callInternal(Callback &callback, QVariant state, ~s fault, ~s result)~n"
			  "    {~n"
			  "        if(connect(this, SIGNAL(callbackSignal(QVariant, ~s, ~s)), callback.target, callback.method))~n"
			  "        {~n"
			  "            emit callbackSignal(state, fault, result);~n"
			  "            disconnect();~n"
			  "        }~n"
			  "        else~n"
			  "	   {~n"
			  "            CallbackCallerTools::onConnectFail(\"~s\", SIGNAL(callbackSignal(QVariant, ~s, ~s)), callback.method);~n"
			  "        }~n"
			  "    }~n"
			  "};~n~n", [ClassName, TypeDef, ClassName, FaultDefault, FaultTypeDef, ClassName, ResultDefault, FaultTypeDef, TypeDef, FaultTypeDef, TypeDef, FaultTypeDef, TypeDef, ClassName, FaultTypeDef, TypeDef]);
	    true ->
		ok
	end,

	io:format(IoDevice,
		  "}~n~n"
		  "#endif // ~s_H", [DefineSymbol]),

	if
	    MakeIntermediate ->
		% generate real class template
		io:format(IoDevice,
			  "~n~n"
			  "// template of a real class~n~n"
			  "/**************************************************~n~n", []),
		TemplateDefineSymbol = get_define_symbol(ClassName),
		io:format(IoDevice,
			  "#ifndef ~s_H~n"
			  "#define ~s_H~n~n"
			  "#include \"~s\"~n~n"
			  "namespace ~s~n"
			  "{~n~n"
			  "class ~s : public ~s~n"
			  "{~n"
			  "public:~n"
			  "    ~s() : ~s()~n"
			  "    {~n"
			  "    }~n"
			  "    ~s(", [TemplateDefineSymbol, TemplateDefineSymbol, get_file_name(GeneratedClassName, "h"), NamespaceName, ClassName, GeneratedClassName, ClassName, GeneratedClassName, ClassName]),
		write_field_ctor_definitions(IoDevice, Model, FieldsInfo),
		io:format(IoDevice,
			  ") : ~s(", [GeneratedClassName]),
		write_field_ctor_vars(IoDevice, Model, FieldsInfo),
		io:format(IoDevice,
			  ")~n"
			  "    {~n"
			  "    }~n"
			  "    virtual ~~~s()~n"
			  "    {~n"
			  "    }~n~n", [ClassName]),
		if
		    IsSharedType ->
			if
			    IsNullable ->
				io:format(IoDevice,
					  "protected:~n"
					  "    ~s(~s* dataRef) : ~s(dataRef)~n"
					  "    {~n"
					  "    }~n~n"
					  "public:~n", [ClassName, DataClassName, GeneratedClassName]),
				write_new_empty_shared_nullable(IoDevice, ClassName, DataClassName);
			    true ->
				io:format(IoDevice,
					  "public:~n", []),
				write_new_empty(IoDevice, ClassName, true)
			end;
		    true ->
			io:format(IoDevice,
				  "public:~n", []),
			write_new_empty(IoDevice, ClassName, false)
		end,
		io:format(IoDevice,
			  "    // below are added methods~n~n~n~n"
			  "};~n~n"
			  "}~n~n"
			  "#endif // ~s_H~n~n"
			  "**************************************************/~n", [TemplateDefineSymbol]);
	    true ->
		ok
	end
    after
	file:close(IoDevice)
    end;
generate_h(_Model, #enum{name = TypeName, values = Values} = _TypeInfo) ->
    ClassName = get_class_name(TypeName),

    {ok, IoDevice} = file:open(filename:join(?OutPath, get_file_name(ClassName, "h")), [write]),
    try
	NamespaceName = ?NamespaceName,
	DefineSymbol = get_define_symbol(ClassName),    
	io:format(IoDevice,
		  "#ifndef ~s_H~n"
		  "#define ~s_H~n~n", [DefineSymbol, DefineSymbol]),
	io:format(IoDevice,
		  "namespace ~s~n"
		  "{~n~n"
		  "class ~s~n"
		  "{~n~n"
		  "public:~n"
		  "enum ~sEnum~n"
		  "{~n", [NamespaceName, ClassName, ClassName]),
	write_enum_values(IoDevice, Values),
	io:format(IoDevice,
		  "};~n~n"
		  "};~n~n"
		  "}~n~n"
		  "#endif // ~s_H", [DefineSymbol])
    after
	file:close(IoDevice)
    end.

generate_cpp(Model, #struct{name = TypeName, fields = FieldsInfo, id = TypeId, is_result_for = CallType, is_fault = IsFault, cpp_options = CppOptions} = TypeInfo) ->
    MakeIntermediate = get_cpp_make_intermediate(CppOptions),
    GeneratedClassName = get_class_name(TypeName, MakeIntermediate),

    {ok, IoDevice} = file:open(filename:join(?OutPath, get_file_name(GeneratedClassName, "cpp")), [write]),
    try
	MakeIntermediate = get_cpp_make_intermediate(CppOptions),

	NamespaceName = ?NamespaceName,
	TopClassName = get_class_name(?TopClass),
	BaseClassName = TopClassName,

	ClassName = get_class_name(TypeName),
	GeneratedDataClassName = ClassName ++ "Data",
	DataClassName = ClassName ++ "Data",

	IsSharedType = is_shared_type(Model, TypeInfo),
	IsNullable = is_nullable(Model, TypeInfo),
	IsResult = (CallType =/= undefined),
	io:format(IoDevice,
		  "#include \"listwrapperobjects.h\"~n"
		  "#include \"~s\"~n~n"
		  "namespace ~s~n"
		  "{~n~n", [get_file_name(GeneratedClassName, "h"), NamespaceName]),

	if
	    IsSharedType ->
		io:format(IoDevice,
			  "~s::~s() : QSharedData()~n"
			  "{~n", [GeneratedDataClassName, GeneratedDataClassName]),
		write_field_defaults(IoDevice, Model, FieldsInfo),
		io:format(IoDevice,
			  "}~n", []),
		case FieldsInfo of
		    [] ->
			ok;
		    _ ->
			io:format(IoDevice,
				  "~s::~s(", [GeneratedDataClassName, GeneratedDataClassName]),
			write_field_ctor_definitions(IoDevice, Model, FieldsInfo),
			io:format(IoDevice,
				  ") : QSharedData()~n"
				  "{~n", []),
			write_field_ctor_assignments(IoDevice, Model, FieldsInfo),
			io:format(IoDevice,
				  "}~n", [])
		end,
		io:format(IoDevice,
			  "~n"
			  "~s::~~~s()~n"
			  "{~n", [GeneratedDataClassName, GeneratedDataClassName]),
		write_delete_instructions(IoDevice, Model, FieldsInfo),
		io:format(IoDevice,
			  "}~n~n", []),
		io:format(IoDevice,
			  "~s::~s() : ~s()~n"
			  "{~n", [GeneratedClassName, GeneratedClassName, BaseClassName]),
		if
		    IsNullable ->
			ok;
		    true ->
			io:format(IoDevice,
				  "    d = new ~s();~n", [DataClassName])
		end,
		io:format(IoDevice,
			  "}~n", []),
		case FieldsInfo of
		    [] ->
			ok;
		    _ ->
			io:format(IoDevice,
				  "~s::~s(", [GeneratedClassName, GeneratedClassName]),
			write_field_ctor_definitions(IoDevice, Model, FieldsInfo),
			io:format(IoDevice,
				  ") : ~s()~n"
				  "{~n"
				  "    d = new ~s(", [BaseClassName, DataClassName]),
			write_field_ctor_vars(IoDevice, Model, FieldsInfo),
			io:format(IoDevice,
				  ");~n"
				  "}~n", [])
		end,
		io:format(IoDevice,
			  "~n"
			  "~s::~~~s()~n"
			  "{~n"
			  "}~n~n", [GeneratedClassName, GeneratedClassName]);
	    true ->
		io:format(IoDevice,
			  "~s::~s() : ~s()~n"
			  "{~n", [GeneratedClassName, GeneratedClassName, BaseClassName]),
		write_field_defaults(IoDevice, Model, FieldsInfo),
		io:format(IoDevice,
			  "}~n", []),
		case FieldsInfo of
		    [] ->
			ok;
		    _ ->
			io:format(IoDevice,
				  "~s::~s(", [GeneratedClassName, GeneratedClassName]),
			write_field_ctor_definitions(IoDevice, Model, FieldsInfo),
			io:format(IoDevice,
				  ") : ~s()~n"
				  "{~n", [BaseClassName]),
			write_field_ctor_assignments(IoDevice, Model, FieldsInfo),
			io:format(IoDevice,
				  "}~n", [])
		end,
		io:format(IoDevice,
			  "~n"
			  "~s::~~~s()~n"
			  "{~n", [GeneratedClassName, GeneratedClassName]),
		write_delete_instructions(IoDevice, Model, FieldsInfo),
		io:format(IoDevice,
			  "}~n~n", [])
	end,

	if
	    IsResult ->
		io:format(IoDevice,
			  "void ~s::resultCall(Callback callback, QVariant state)~n"
			  "{~n"
			  "    ~sCallbackCaller::call(callback, state, getResult());~n"
			  "}~n~n", [ClassName, ClassName]);
	    true ->
		ok
	end,

	if
	    IsFault ->
		io:format(IoDevice,
			  "void ~s::resultFaultCall(~s* server, Callback callback, QVariant state, qint32 resultTypeId)~n"
			  "{~n"
			  "    server->resultFaultCall(callback, state, ~sthis, resultTypeId);~n"
			  "}~n~n", [ClassName, ?ServerProxyBaseClassName, if IsSharedType -> "*"; true -> "" end]);
	    true ->
		ok
	end,

	write_get_set(IoDevice, Model, IsSharedType, IsNullable, GeneratedClassName, FieldsInfo),
	io:format(IoDevice,
		  "~n"
		  "qint32 ~s::getRepresentedTypeId()~n"
		  "{~n"
		  "    return ~b;~n"
		  "}~n"
		  "~n"
		  "qint32 ~s::getTypeId() const~n"
		  "{~n"
		  "    return ~b;~n"
		  "}~n"
		  "void ~s::writeProperties(PropertyWriter *writer)~n"
		  "{~n"
		  "    ~s::writeProperties(writer);~n~n", [GeneratedClassName, TypeId, GeneratedClassName, TypeId, GeneratedClassName, BaseClassName]),
	write_write_instructions(IoDevice, Model, IsSharedType, FieldsInfo),
	io:format(IoDevice,
		  "}~n"
		  "PropertyReadResult ~s::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)~n"
		  "{~n"
		  "    PropertyReadResult result = ~s::readProperty(propertyId, typeId, reader);~n"
		  "    if(result.getIsPropertyFound())~n"
		  "        return result;~n~n", [GeneratedClassName, BaseClassName]),
	case FieldsInfo of
	    [_SomeElement | _] ->
		io:format(IoDevice,
			  "    switch(propertyId)~n"
			  "    {~n", []),
		write_read_cases(IoDevice, Model, IsSharedType, FieldsInfo),
		io:format(IoDevice,
			  "    }~n~n", []);
	    _NoElements ->
		ok
	end,
	io:format(IoDevice,
		  "    return PropertyReadResult(false);~n"
		  "}~n", []),
	io:format(IoDevice,
		  "~n"
		  "}~n", [])
    after
	file:close(IoDevice)
    end;
generate_cpp(_Model, #enum{} = _TypeInfo) ->
    ok.


%%% write functions for model

write_model_fill_instructions_types(_IoDevice, [] = _TypesInfo) ->
    ok;
write_model_fill_instructions_types(IoDevice, [#enum{id = EnumId, values = EnumValues} = _TypeInfo | Rest]) ->
    io:nl(IoDevice),
    write_model_fill_instructions_enum_values(IoDevice, EnumId, EnumValues),
    io:nl(IoDevice),
    write_model_fill_instructions_types(IoDevice, Rest);
write_model_fill_instructions_types(IoDevice, [#struct{id = TypeId, name = TypeName} = _TypeInfo | Rest]) ->
    io:format(IoDevice, "    addTypeInfo(TypeInfo(~b, \"~s\"));~n", [TypeId, TypeName]),
    write_model_fill_instructions_types(IoDevice, Rest).

write_model_fill_instructions_enum_values(_IoDevice, _EnumId, [] = _EnumValues) ->
    ok;
write_model_fill_instructions_enum_values(IoDevice, EnumId, [{ValueName, ValueId} = _EnumValueInfo | Rest] = _EnumValues) ->
    io:format(IoDevice, "    addEnumValueInfo(EnumValueInfo(~b, ~b, \"~s\"));~n", [EnumId, ValueId, ValueName]),
    write_model_fill_instructions_enum_values(IoDevice, EnumId, Rest).

write_model_fill_instructions_properties(_IoDevice, _Model, [] = _TypesInfo) ->
    ok;
write_model_fill_instructions_properties(IoDevice, Model, [#enum{} = _TypeInfo | Rest]) ->
    write_model_fill_instructions_properties(IoDevice, Model, Rest);
write_model_fill_instructions_properties(IoDevice, Model, [#struct{fields = FieldsInfo} = TypeInfo | Rest]) ->
    write_model_fill_instructions_properties(IoDevice, Model, TypeInfo, FieldsInfo),
    write_model_fill_instructions_properties(IoDevice, Model, Rest).

write_model_fill_instructions_properties(_IoDevice, _Model, _TypeInfo, [] = _FieldsInfo) ->
    ok;
write_model_fill_instructions_properties(IoDevice, Model, #struct{id = TypeId} = TypeInfo, [#field{name = FieldName, id = FieldId} = FieldInfo | Rest]) ->
    FieldType = get_field_type(FieldInfo),
    io:format(IoDevice, "    addPropertyInfo(PropertyInfo(~b, ~b, \"~s\", ~s, ~s));~n", [TypeId, FieldId, FieldName, is_custom_type_or_list(Model, FieldType), is_record(FieldType, union)]),
    write_model_fill_instructions_properties(IoDevice, Model, TypeInfo, Rest).


%%% write functions for list wrappers

write_list_wrappers(_IoDevice, _Model, _BaseClassName, [] = _Types) ->
    ok;
write_list_wrappers(IoDevice, Model, BaseClassName, [Type | Rest] = _Types) ->
    ReadType = get_read_type(Type),
    ClassName = get_list_wrapper_name(ReadType),
    TypeName = get_type_definition(Model, Type),
    io:format(IoDevice,
	      "class ~s : public ~s<~s>~n"
	      "{~n"
	      "public:~n"
	      "    ~s(QList<~s> *list, PropertyInfo itemPropertyInfo) : ~s<~s>(list, itemPropertyInfo)~n"
	      "    {~n"
	      "    }~n"
	      "    virtual ~~~s()~n"
	      "    {~n"
	      "    }~n~n"
	      "    virtual PropertyReadResult readListItem(PropertyReader *reader)~n"
	      "    {~n"
	      "        list->append(reader->read~s());~n~n"
	      "        return PropertyReadResult(true);~n"
	      "    }~n"
	      "};~n~n", [ClassName, BaseClassName, TypeName, ClassName, TypeName, BaseClassName, TypeName, ClassName, ReadType]),
    write_list_wrappers(IoDevice, Model, BaseClassName, Rest).


%%% write functions for server proxy

write_required_types(IoDevice, Model, RequiredTypes, AdditionalTypes) ->
    NewRequiredTypes = add_additional_types(Model, AdditionalTypes, RequiredTypes),
    RequiredTypesUpdated = dict:map(fun(Key, Value) -> write_std_include(IoDevice, Key, Value) end, NewRequiredTypes),
    io:nl(IoDevice),
    dict:map(fun(Key, Value) -> if Value == std -> ok; true -> write_include(IoDevice, Key, Value) end, undefined end, RequiredTypesUpdated).

add_additional_types(_Model, [] = _AdditionalTypes, Result) ->
    Result;
add_additional_types(Model, [AdditionalType | Rest] = _AdditionalTypes, Result) ->
    NewResult = add_required_type(Model, AdditionalType, Result),
    add_additional_types(Model, Rest, NewResult).


write_method_defs(_IoDevice, _Model, [] = _TypesInfo) ->
    ok;
write_method_defs(IoDevice, Model, [#struct{return_type = ReturnType} = TypeInfo | Rest] = _TypesInfo) when ReturnType =/= undefined ->
    write_method_def(IoDevice, Model, TypeInfo),
    write_method_defs(IoDevice, Model, Rest);
write_method_defs(IoDevice, Model, [_TypeInfo | Rest] = _TypesInfo) ->
    write_method_defs(IoDevice, Model, Rest).

write_method_def(IoDevice, Model, #struct{name = TypeName, fields = FieldsInfo} = _TypeInfo) ->
    MethodName = get_method_name(TypeName),

    io:format(IoDevice,
	      "    void ~s(Callback callback~s", [MethodName, if FieldsInfo == [] -> ""; true -> ", " end]),
    write_field_ctor_definitions(IoDevice, Model, FieldsInfo),
    io:format(IoDevice,
	      ");~n", []),

    io:format(IoDevice,
	      "    void ~s(Callback callback, QVariant state~s", [MethodName, if FieldsInfo == [] -> ""; true -> ", " end]),
    write_field_ctor_definitions(IoDevice, Model, FieldsInfo),
    io:format(IoDevice,
	      ");~n", []).

write_methods(_IoDevice, _Model, _ClassName, [] = _TypesInfo) ->
    ok;
write_methods(IoDevice, Model, ClassName, [#struct{return_type = ReturnType} = TypeInfo | Rest] = _TypesInfo) when ReturnType =/= undefined ->
    write_method(IoDevice, Model, ClassName, TypeInfo),
    write_methods(IoDevice, Model, ClassName, Rest);
write_methods(IoDevice, Model, ClassName, [_TypeInfo | Rest] = _TypesInfo) ->
    write_methods(IoDevice, Model, ClassName, Rest).

write_method(IoDevice, Model, ClassName, #struct{name = TypeName, fields = FieldsInfo} = _TypeInfo) ->
    MethodName = get_method_name(TypeName),
    MethodClassName = get_class_name(TypeName),

    io:format(IoDevice,
	      "void ~s::~s(Callback callback~s", [ClassName, MethodName, if FieldsInfo == [] -> ""; true -> ", " end]),
    write_field_ctor_definitions(IoDevice, Model, FieldsInfo),
    io:format(IoDevice,
	      ")~n"
	      "{~n"
	      "    ~s(callback, QVariant()~s", [MethodName, if FieldsInfo == [] -> ""; true -> ", " end]),
    write_field_ctor_vars(IoDevice, Model, FieldsInfo),
    io:format(IoDevice,
	      ");~n"
	      "}~n~n", []),

    io:format(IoDevice,
	      "void ~s::~s(Callback callback, QVariant state~s", [ClassName, MethodName, if FieldsInfo == [] -> ""; true -> ", " end]),
    write_field_ctor_definitions(IoDevice, Model, FieldsInfo),
    io:format(IoDevice,
	      ")~n"
	      "{~n"
	      "    send(callback, state, ", []),
    if
	FieldsInfo == [] ->
	    io:format(IoDevice,
		      "~s::___new_(", [MethodClassName]);
	true ->
	    io:format(IoDevice,
		      "new ~s(", [MethodClassName]),
	    write_field_ctor_vars(IoDevice, Model, FieldsInfo)
    end,
    io:format(IoDevice,
	      "));~n"
	      "}~n~n", []).

get_result_types_info(Model) ->
    get_result_types_info(?model:get_types_info(Model), []).
get_result_types_info([] = _TypesInfo, Result) ->
    lists:reverse(Result);
get_result_types_info([#struct{is_result_for = CallType} = TypeInfo | Rest] = _TypesInfo, Result) when CallType =/= undefined ->
    get_result_types_info(Rest, [TypeInfo | Result]);
get_result_types_info([_TypeInfo | Rest] = _TypesInfo, Result) ->
    get_result_types_info(Rest, Result).

write_result_fault_cases(_IoDevice,  [] = _TypesInfo) ->
    ok;
write_result_fault_cases(IoDevice, [#struct{is_result_for = CallType} = TypeInfo | Rest] = _TypesInfo) when CallType =/= undefined ->
    write_result_fault_case(IoDevice, TypeInfo),
    write_result_fault_cases(IoDevice, Rest);
write_result_fault_cases(IoDevice, [_TypeInfo | Rest] = _TypesInfo) ->
    write_result_fault_cases(IoDevice, Rest).

write_result_fault_case(IoDevice, #struct{name = TypeName, id = TypeId} = _TypeInfo) ->
    ResultClassName = get_class_name(TypeName),
    io:format(IoDevice,
          "        case ~b:~n"
          "            ~sCallbackCaller::call(callback, state, fault);~n"
          "            break;~n", [TypeId, ResultClassName]).


%%% write functions for h

write_std_include(IoDevice, Type, Value) ->
    StdInclude = case Value of std -> Type; _ -> get_std_include(Type) end,
    case StdInclude of
    undefined ->
        undefined;
    _ ->
        io:format(IoDevice, "#include <~s>~n", [StdInclude]),
        std
    end.
write_include(IoDevice, Type, _Value) ->
    io:format(IoDevice, "#include \"~s\"~n", [get_file_name(get_class_name(Type), "h")]).

write_get_set_defs(_IoDevice, _Model, [] = _FieldsInfo) ->
    ok;
write_get_set_defs(IoDevice, Model, [#field{name = FieldName} = FieldInfo | Rest]) ->
    MethodName = get_get_set_method_name_part(FieldName),
    FieldType = get_field_type(FieldInfo),
    TypeDef = get_type_definition(Model, FieldType),
    io:format(IoDevice,
          "    ~s get~s() const;~n"
          "    void set~s(~s value);~n", [TypeDef, MethodName, MethodName, TypeDef]),
    write_get_set_defs(IoDevice, Model, Rest).

write_fields(_IoDevice, _Model, [] = _FieldsInfo) ->
    ok;
write_fields(IoDevice, Model, [#field{name = FieldName} = FieldInfo | Rest]) ->
    FieldType = get_field_type(FieldInfo),
    io:format(IoDevice,
          "    ~s ~s;~n", [get_type_definition(Model, FieldType), get_field_name(FieldName)]),
    write_fields(IoDevice, Model, Rest).

write_enum_values(_IoDevice, [] = _Values) ->
    ok;
write_enum_values(IoDevice, [{EnumValueName, EnumValueId} = _Value | Rest] = _Values) ->
    io:format(IoDevice,
          "    ~s = ~b", [get_enum_value_name(EnumValueName), EnumValueId]),
    case Rest of
    [] ->
        io:format(IoDevice, "~n", []);
    _Something ->
        io:format(IoDevice, ",~n", [])
    end,
    write_enum_values(IoDevice, Rest).

write_new_empty_shared_nullable(IoDevice, ClassName, DataClassName) ->
    io:format(IoDevice,
          "    // never use ___new_ in your code!!!~n"
          "    static ~s* ___new_()~n"
          "    {~n"
          "        return new ~s(new ~s());~n"
          "    }~n"
          "    static ~s empty()~n"
          "    {~n"
          "        return ~s(new ~s());~n"
          "    }~n~n", [ClassName, ClassName, DataClassName, ClassName, ClassName, DataClassName]).
write_new_empty(IoDevice, ClassName, IsSharedType) ->
    io:format(IoDevice,
          "    // never use ___new_ in your code!!!~n"
          "    static ~s* ___new_()~n"
          "    {~n"
          "        return new ~s();~n"
          "    }~n", [ClassName, ClassName]),
    if
    IsSharedType ->
        io:format(IoDevice,
              "    static ~s empty()~n"
              "    {~n"
              "        return ~s();~n"
              "    }~n~n", [ClassName, ClassName]);
    true ->
        ok
    end.


%%% write functions for cpp

write_field_ctor_definitions(_IoDevice, _Model, [] = _FieldsInfo) ->
    ok;
write_field_ctor_definitions(IoDevice, Model, [#field{name = FieldName} = FieldInfo | Rest]) ->
    FieldType = get_field_type(FieldInfo),
    io:format(IoDevice, "~s ~s", [get_type_definition(Model, FieldType), get_field_name(FieldName)]),
    case Rest of
    [] ->
        ok;
    _Other ->
        io:format(IoDevice, ", ", [])
    end,
    write_field_ctor_definitions(IoDevice, Model, Rest).

write_field_ctor_vars(_IoDevice, _Model, [] = _FieldsInfo) ->
    ok;
write_field_ctor_vars(IoDevice, Model, [#field{name = FieldName} = _FieldInfo | Rest]) ->
    io:format(IoDevice, "~s", [get_field_name(FieldName)]),
    case Rest of
    [] ->
        ok;
    _Other ->
        io:format(IoDevice, ", ", [])
    end,
    write_field_ctor_vars(IoDevice, Model, Rest).

write_field_ctor_assignments(_IoDevice, _Model, [] = _FieldsInfo) ->
    ok;
write_field_ctor_assignments(IoDevice, Model, [#field{name = FieldName} = _FieldInfo | Rest]) ->
    Field = get_field_name(FieldName),
    io:format(IoDevice, "    this->~s = ~s;~n", [Field, Field]),
    write_field_ctor_assignments(IoDevice, Model, Rest).

write_field_defaults(_IoDevice, _Model, [] = _FieldsInfo) ->
    ok;
write_field_defaults(IoDevice, Model, [#field{name = FieldName, default_value = DefaultValue} = FieldInfo | Rest]) ->
    FieldType = get_field_type(FieldInfo),
    case get_default(Model, FieldType, DefaultValue) of
    undefined ->
        ok;
    Value ->
        io:format(IoDevice,
              "    this->~s = ~s;~n", [get_field_name(FieldName), Value])
    end,
    write_field_defaults(IoDevice, Model, Rest).

write_delete_instructions(_IoDevice, _Model, [] = _FieldsInfo) ->
    ok;
write_delete_instructions(IoDevice, Model, [#field{name = FieldName} = FieldInfo | Rest]) ->
    FieldType = get_field_type(FieldInfo),
    case FieldType of
    #list{type = ListType} -> % note list-in-lists are not supported
        case is_by_ref(Model, ListType) of
        true ->
             case get_read_type(ListType) of
             undefined ->
                 Name = get_field_name(FieldName),
                 io:format(IoDevice,
                       "    qDeleteAll(this->~s.begin(), this->~s.end());~n", [Name, Name]);
             _Other ->
                 ok
             end;
        false ->
            ok
        end;
    _Other ->
        case is_by_ref(Model, FieldType) of
        true ->
            Name = get_field_name(FieldName),
            io:format(IoDevice,
                  "    if(this->~s != NULL)~n"
                  "    {~n"
                  "        delete this->~s;~n"
                  "        this->~s = NULL;~n"
                  "    }~n", [Name, Name, Name]);
        false ->
            ok
        end
    end,
    write_delete_instructions(IoDevice, Model, Rest).

write_get_set(_IoDevice, _Model, _IsSharedType, _IsNullable, _ClassName, [] = _FieldsInfo) ->
    ok;
write_get_set(IoDevice, Model, IsSharedType, IsNullable, ClassName, [#field{name = FieldName} = FieldInfo | Rest]) ->
    FieldType = get_field_type(FieldInfo),
    MethodName = get_get_set_method_name_part(FieldName),
    TypeDef = get_type_definition(Model, FieldType),
    Field = get_field_name(FieldName),
    SourceObject = if IsSharedType -> "this->d"; true -> "this" end,
    io:format(IoDevice,
          "~s ~s::get~s() const~n"
          "{~n", [TypeDef, ClassName, MethodName]),
    if
    IsNullable ->
        io:format(IoDevice,
              "    Q_ASSERT_X(!isNull(), \"~s::get~s\", \"Getter call on object which isNull\");~n", [ClassName, MethodName]);
    true ->
        ok
    end,
    io:format(IoDevice,
          "    return ~s->~s;~n"
          "}~n"
          "void ~s::set~s(~s value)~n"
          "{~n", [SourceObject, Field, ClassName, MethodName, TypeDef]),
    if
    IsNullable ->
        io:format(IoDevice,
              "    Q_ASSERT_X(!isNull(), \"~s::set~s\", \"Setter call on object which isNull\");~n", [ClassName, MethodName]);
    true ->
        ok
    end,
    io:format(IoDevice,
          "    ~s->~s = value;~n"
          "}~n", [SourceObject, Field]),
    write_get_set(IoDevice, Model, IsSharedType, IsNullable, ClassName, Rest).

write_write_instructions(_IoDevice, _Model, _IsSharedType, [] = _FieldsInfo) ->
    ok;
write_write_instructions(IoDevice, Model, IsSharedType, [#field{name = FieldName, id = FieldId, type = #list{}} = FieldInfo | Rest]) ->
    #list{type = ListType} = get_field_type(FieldInfo),
    Field = get_field_name(FieldName),
    SourceObject = if IsSharedType -> "this->d"; true -> "this" end,
    case ?model:get_type_info_silently_for_cpp_generator(Model, ListType) of
    #enum{id = EnumId} ->
        io:format(IoDevice,
              "    TransportableListOfEnumWrapper<~s> ~s_wrapper(~s->~s, ~b);~n"
              "    writer->writeProperty(this, ~b, &~s_wrapper);~n",
              [get_type_definition(Model, ListType), Field, SourceObject, Field, EnumId, FieldId, Field]);
    ListTypeInfo ->
         case get_read_type(ListType) of
         undefined ->
             case is_shared_type(Model, ListTypeInfo) of
             true ->
                 io:format(IoDevice,
                       "    TransportableListOfSharedWrapper<~s> ~s_wrapper(~s->~s);~n"
                       "    writer->writeProperty(this, ~b, &~s_wrapper);~n",
                       [get_class_name(ListType), Field, SourceObject, Field, FieldId, Field]);
             false ->
                 io:format(IoDevice,
                       "    TransportableListWrapper<~s*> ~s_wrapper(~s->~s);~n"
                       "    writer->writeProperty(this, ~b, &~s_wrapper);~n",
                       [get_class_name(ListType), Field, SourceObject, Field, FieldId, Field])
             end;
         _ReadType ->
             io:format(IoDevice,
                   "    writer->writeProperty(this, ~b, ~s->~s);~n", [FieldId, SourceObject, Field])
         end
    end,
    write_write_instructions(IoDevice, Model, IsSharedType, Rest);
write_write_instructions(IoDevice, Model, IsSharedType, [#field{name = FieldName, id = FieldId} = FieldInfo | Rest]) ->
    FieldType = get_field_type(FieldInfo),
    FieldTypeInfo = ?model:get_type_info_silently_for_cpp_generator(Model, FieldType),
    SourceObject = if IsSharedType -> "this->d"; true -> "this" end,
    case FieldTypeInfo of
    #enum{id = EnumId} ->
        io:format(IoDevice,
              "    writer->writeEnumProperty(this, ~b, ~b, ~s->~s);~n", [FieldId, EnumId, SourceObject, get_field_name(FieldName)]);
    _Other ->
        case is_shared_type(Model, FieldTypeInfo) of
        true ->
            io:format(IoDevice,
                  "    writer->writeProperty(this, ~b, &~s->~s);~n", [FieldId, SourceObject, get_field_name(FieldName)]);
        false ->
            io:format(IoDevice,
                  "    writer->writeProperty(this, ~b, ~s->~s);~n", [FieldId, SourceObject, get_field_name(FieldName)])
        end
    end,
    write_write_instructions(IoDevice, Model, IsSharedType, Rest).

write_read_cases(_IoDevice, _Model, _IsSharedType, [] = _FieldsInfo) ->
    ok;
write_read_cases(IoDevice, Model, IsSharedType, [#field{name = FieldName, id = FieldId} = FieldInfo | Rest]) ->
    FieldType = get_field_type(FieldInfo),
    io:format(IoDevice,
          "        case ~b:~n", [FieldId]),
    Field = get_field_name(FieldName),
    SourceObject = if IsSharedType -> "this->d"; true -> "this" end,
    case get_read_type(FieldType) of
    undefined ->
        case FieldType of
        #union{types = Types} ->
            io:format(IoDevice,
                  "            ~s->~s = NULL;~n"
                  "            switch(typeId)~n"
                  "            {~n", [SourceObject, Field]),
            write_new_instance_cases(IoDevice, Model, Field, Types),
            io:format(IoDevice,
                  "            }~n", []),
            io:format(IoDevice,
                  "            return PropertyReadResult(~s->~s);~n", [SourceObject, Field]);
        #list{type = ListType} ->
            case ?model:get_type_info_silently_for_cpp_generator(Model, ListType) of
            #enum{id = EnumId} ->
                EnumName = get_type_definition(Model, ListType),
                io:format(IoDevice,
                      "            ~s->~s = QList<~s>();~n"
                      "            return PropertyReadResult(new ListOfEnumWrapperObject<~s>(&~s->~s, PropertyInfo(false, false), ~b));~n",
                      [SourceObject, Field, EnumName, EnumName, SourceObject, Field, EnumId]);
            ListTypeInfo ->
                case get_read_type(ListType) of
                undefined ->
                    case is_shared_type(Model, ListTypeInfo) of
                    true ->
                        io:format(IoDevice,
                              "            ~s->~s = QList<~s>();~n"
                              "            return PropertyReadResult(new ListOfSharedWrapperObject<~s>(&~s->~s, PropertyInfo(~s, ~s)));~n",
                              [SourceObject, Field, get_type_definition(Model, ListType), get_type_definition(Model, ListType, true), SourceObject, Field, is_custom_type_or_list(Model, ListType), is_record(ListType, union)]);
                    false ->
                        io:format(IoDevice,
                              "            ~s->~s = QList<~s>();~n"
                              "            return PropertyReadResult(new ListWrapperObject<~s>(&~s->~s, PropertyInfo(~s, ~s)));~n",
                              [SourceObject, Field, get_type_definition(Model, ListType), get_type_definition(Model, ListType, true), SourceObject, Field, is_custom_type_or_list(Model, ListType), is_record(ListType, union)])
                    end;
                ReadType ->
                    io:format(IoDevice,
                          "            ~s->~s = QList<~s>();~n"
                          "            return PropertyReadResult(new ~s(&~s->~s, PropertyInfo(false, false)));~n",
                          [SourceObject, Field, get_type_definition(Model, ListType), get_list_wrapper_name(ReadType), SourceObject, Field])
                end
            end;
        _Other ->
            FieldTypeInfo = ?model:get_type_info_silently_for_cpp_generator(Model, FieldType),
            case FieldTypeInfo of
            #enum{id = EnumId} ->
                io:format(IoDevice,
                      "            ~s->~s = (~s)reader->readEnum(~b);~n"
                      "            return PropertyReadResult(true);~n", [SourceObject, Field, get_type_definition(Model, FieldType), EnumId]);
            _SomethingElse ->
                case is_shared_type(Model, FieldTypeInfo) of
                true ->
                    case is_nullable(Model, FieldTypeInfo) of
                    true ->
                        io:format(IoDevice,
                              "            ~s->~s = ~s::empty();~n", [SourceObject, Field, get_class_name(FieldType)]);
                    false ->
                        ok
                    end,
                    io:format(IoDevice,
                          "            return PropertyReadResult(&~s->~s);~n", [SourceObject, Field]);
                false ->
                    io:format(IoDevice,
                          "            ~s->~s = new ~s();~n"
                          "            return PropertyReadResult(~s->~s);~n", [SourceObject, Field, get_class_name(FieldType), SourceObject, Field])
                end
            end
        end;
    ReadType ->
        io:format(IoDevice,
              "            ~s->~s = reader->read~s();~n"
              "            return PropertyReadResult(true);~n", [SourceObject, Field, ReadType])
    end,
    write_read_cases(IoDevice, Model, IsSharedType, Rest).

write_new_instance_cases(_IoDevice, _Model,_Field, [] = _Types) ->
    ok;
write_new_instance_cases(IoDevice, Model, Field, [Type | Rest]) ->
    TypeInfo = ?model:get_type_info_for_cpp_generator(Model, Type),
    #struct{id = TypeId, name = TypeName} = TypeInfo,
    IsSharedType = is_shared_type(Model, TypeInfo),
    SourceObject = if IsSharedType -> "this->d"; true -> "this" end,
    io:format(IoDevice,
          "                case ~b:~n", [TypeId]),
    case IsSharedType andalso is_nullable(Model, TypeInfo) of
    true ->
        io:format(IoDevice,
              "                    ~s->~s = ~s::___new_();~n", [SourceObject, Field, get_class_name(TypeName)]);
    false ->
        io:format(IoDevice,
              "                    ~s->~s = new ~s();~n", [SourceObject, Field, get_class_name(TypeName)])
    end,
    io:format(IoDevice,
          "                    break;~n", []),
    write_new_instance_cases(IoDevice, Model, Field, Rest).


%%% info functions for list wrappers

get_list_wrapper_name(ReadType) ->
    io_lib:format(?ListWrappersClassNameFormat, [ReadType]).


%%% info functions for server proxy

get_required_types_for_methods(Model) ->
    get_required_types_for_methods(Model, ?model:get_types_info(Model), dict:new()).
get_required_types_for_methods(_Model, [] = _TypesInfo, Result) ->
    Result;
get_required_types_for_methods(Model, [#struct{name = Type, return_type = ReturnType} = _TypeInfo | Rest] = _TypesInfo, Result) when ReturnType =/= undefined ->
    NewResult = add_required_type(Model, Type, Result),
    get_required_types_for_methods(Model, Rest, NewResult);
get_required_types_for_methods(Model, [_TypeInfo | Rest] = _TypesInfo, Result) ->
    get_required_types_for_methods(Model, Rest, Result).

get_required_types_for_results(Model) ->
    get_required_types_for_results(Model, ?model:get_types_info(Model), dict:new()).
get_required_types_for_results(_Model, [] = _TypesInfo, Result) ->
    Result;
get_required_types_for_results(Model, [#struct{name = Type, is_result_for = CallType} = _TypeInfo | Rest] = _TypesInfo, Result) when CallType =/= undefined ->
    NewResult = add_required_type(Model, Type, Result),
    get_required_types_for_results(Model, Rest, NewResult);
get_required_types_for_results(Model, [_TypeInfo | Rest] = _TypesInfo, Result) ->
    get_required_types_for_results(Model, Rest, Result).


%%% info functions for h

get_required_types(Model, FieldsInfo) ->
    get_required_types(Model, FieldsInfo, dict:new()).
get_required_types(_Model, [], Result) ->
    Result;
get_required_types(Model, [#field{} = FieldInfo | Rest], Result) ->
    FieldType = get_field_type(FieldInfo),
    get_required_types(Model, Rest, add_required_type(Model, FieldType, Result)).
add_required_type(Model, #list{type = ListType}, Result) ->
    NewResult = add_required_type(Model, ListType, Result),
    add_required_type(Model, list, NewResult);
add_required_type(Model, #union{types = Types}, Result) ->
    NewResult = add_required_types(Model, Types, Result),
    add_required_type(Model, ?TopClass, NewResult);
add_required_type(Model, Type, Result) ->
    case get_std_include(Type) of
    skip ->
        Result;
    _Else ->
        NewResult = dict:store(Type, undefined, Result),
        case get_cpp_options(Model, Type) of
        #cpp_class_options{include_lib = IncludeLib} when IncludeLib =/= undefined ->
            dict:store(IncludeLib, std, NewResult);
        _ ->
            NewResult
        end        
    end.
add_required_types(_Model, [], Result) ->
    Result;
add_required_types(Model, [Type | Rest], Result) ->
    add_required_types(Model, Rest, add_required_type(Model, Type, Result)).

get_file_name(ClassName, Extension) ->
    string:to_lower(ClassName) ++ [$. | Extension].
get_define_symbol(ClassName) ->
    string:to_upper(ClassName).
get_class_name(TypeName) ->
    ?model:to_pascal_case(atom_to_list(TypeName)).
get_class_name(TypeName, true = _IsIntermediate) ->
    get_class_name(TypeName) ++ "Intermediate";
get_class_name(TypeName, _IsIntermediate) ->
    get_class_name(TypeName).
get_field_name(FieldName) ->
    ?model:to_camel_case(atom_to_list(FieldName)).
get_get_set_method_name_part(FunctionName) ->
    ?model:to_pascal_case(atom_to_list(FunctionName)).
get_method_name(FunctionName) ->
    ?model:to_camel_case(atom_to_list(FunctionName)).
get_enum_value_name(EnumValueName) ->
    ?model:to_pascal_case(atom_to_list(EnumValueName)).
get_enum_value_name_usage(EnumName, EnumValueName) ->
    ?model:to_pascal_case(atom_to_list(EnumName)) ++ "::" ++ ?model:to_pascal_case(atom_to_list(EnumValueName)).

get_std_include({cpp, int64}) ->
    skip;
get_std_include({cpp, date}) ->
    "QDate";
get_std_include(string) ->
    "QString";
get_std_include(datetime) ->
    "QDateTime";
get_std_include(binary) ->
    "QByteArray";
get_std_include(list) ->
    "QList";
get_std_include(Type) ->
    case ?model:is_primitive(Type) of
        true ->
            skip;
        false ->
            undefined
    end.

get_type_definition(Model, Type) ->
    get_type_definition(Model, Type, false).
get_type_definition(_Model, {cpp, int64}, _PreventReferences) ->
    "qint64";
get_type_definition(_Model, {cpp, date}, _PreventReferences) ->
    "QDate";
get_type_definition(_Model, string, _PreventReferences) ->
    "QString";
get_type_definition(_Model, int, _PreventReferences) ->
    "qint32";
get_type_definition(_Model, float, _PreventReferences) ->
    "qreal";
get_type_definition(_Model, bool, _PreventReferences) ->
    "bool";
get_type_definition(_Model, datetime, _PreventReferences) ->
    "QDateTime";
get_type_definition(_Model, binary, _PreventReferences) ->
    "QByteArray";
get_type_definition(Model, #list{type = ListType}, _PreventReferences) ->
    io_lib:format("QList<~s>", [get_type_definition(Model, ListType)]);
get_type_definition(_Model, #union{}, true = _PreventReferences) ->
    get_class_name(?TopClass);
get_type_definition(_Model, #union{} = _Type, false = _PreventReferences) ->
    get_class_name(?TopClass) ++ "*";
get_type_definition(Model, Type, PreventReferences) ->
    case ?model:get_type_info_for_cpp_generator(Model, Type) of
    #enum{cpp_options = CppOptions} ->
        case get_replace_class_name(CppOptions) of
        undefined ->
            get_enum_definition(Type);
        ReplaceClassName ->
            ReplaceClassName
        end;
    TypeInfo ->
        case not(PreventReferences) andalso is_by_ref_info(Model, TypeInfo) of
        true ->
            get_class_name(Type) ++ "*";
        false ->
            get_class_name(Type)
        end
    end.

%% do not use directly
get_enum_definition(Enum) ->
    EnumName = get_class_name(Enum),
    EnumName ++ "::" ++ EnumName ++ "Enum".


%%% info functions for cpp

is_custom_type_or_list(_Model, {cpp, _} = _Type) ->
    false;
is_custom_type_or_list(_Model, #list{} = _Type) ->
    true;
is_custom_type_or_list(Model, Type) ->
    not(?model:is_primitive(Type)) andalso not(?model:is_enum_for_cpp_generator(Model, Type)).

-define(iif_empty(Value, IfTrue, Else), if Value == undefined -> IfTrue; true -> Else end).

get_default(_Model, {cpp, int64}, Value) ->
    ?iif_empty(Value, "0", integer_to_list(Value));
get_default(_Model, {cpp, date}, Value) ->
    ?iif_empty(Value, undefined, throw({error, "Not implemented"})); % @todo implement default conversion
get_default(_Model, string, Value) ->
    ?iif_empty(Value, undefined, io_lib:format("~p", [Value]));
get_default(_Model, int, Value) ->
    ?iif_empty(Value, "0", integer_to_list(Value));
get_default(_Model, float, Value) ->
    ?iif_empty(Value, "0", mochinum:digits(Value));
get_default(_Model, bool, Value) ->
    ?iif_empty(Value, "false", atom_to_list(Value));
get_default(_Model, datetime, Value) ->
    ?iif_empty(Value, undefined, throw({error, "Not implemented"})); % @todo implement default conversion
get_default(_Model, binary, Value) ->
    ?iif_empty(Value, undefined, throw({error, "Not implemented"})); % @todo implement default conversion
get_default(_Model, #list{}, Value) ->
    ?iif_empty(Value, undefined, throw({error, "Not implemented"})); % @todo implement default conversion
get_default(_Model, #union{}, Value) ->
    ?iif_empty(Value, "NULL", throw({error, "Not implemented"})); % @todo implement default conversion
get_default(Model, Type, Value) ->
    case ?model:get_type_info_silently_for_cpp_generator(Model, Type) of
    #enum{name = EnumName, values = [{FirstValueName, _FirstValueId} | _OtherValues], cpp_options = CppOptions} ->
        case get_cpp_default(CppOptions) of
        undefined ->
            ?iif_empty(Value, get_enum_value_name_usage(EnumName, FirstValueName), get_enum_value_name_usage(EnumName, Value)); % @todo implement default conversion
        CppDefault ->
            ?iif_empty(Value, CppDefault, get_enum_value_name_usage(EnumName, Value)) % @todo implement default conversion
        end;
    TypeInfo ->
        case is_shared_type(Model, TypeInfo) of
        true ->
            undefined;
        false ->
            ?iif_empty(Value, "NULL", throw({error, "Not implemented"})) % @todo implement default conversion
        end
    end.

get_default_extended(Model, Type, Value) ->
    case get_default(Model, Type, Value) of
    undefined ->
        io_lib:format("~s()", [get_type_definition(Model, Type, false)]);
    Default ->
        Default
    end.

get_read_types() ->
    [{cpp, int64}, {cpp, date}, string, int, float, bool, datetime, binary].

get_read_type({cpp, int64}) ->
    "Int64";
get_read_type({cpp, date}) ->
    "Date";
get_read_type(string) ->
    "String";
get_read_type(int) ->
    "Int32";
get_read_type(float) ->
    "Double";
get_read_type(bool) ->
    "Bool";
get_read_type(datetime) ->
    "DateTime";
get_read_type(binary) ->
    "Bytes";
get_read_type(_Type) ->
    undefined.


%%% options

get_result_type(#struct{return_type = Type, cpp_options = #cpp_class_options{cpp_return_type = CppType}}) when Type =/= undefined andalso CppType =/= undefined ->
    get_cpp_type(Type, CppType);
get_result_type(#struct{return_type = Type}) ->
    Type.
get_field_type(#field{type = Type, cpp_options = #cpp_field_options{cpp_type = CppType}}) when CppType =/= undefined ->
    get_cpp_type(Type, CppType);
get_field_type(#field{type = Type}) ->
    Type.

get_cpp_type(#list{type = Type}, #list{type = CppType}) ->
    #list{type = get_cpp_type(Type, CppType)};
get_cpp_type(_Type, CppType) ->
    {cpp, CppType}.

get_replace_class_name(undefined = _CppOptions) ->
    undefined;
get_replace_class_name(#cpp_class_options{replace_with = ReplaceClassName} = _CppOptions) ->
    ReplaceClassName.

get_cpp_default(undefined = _CppOptions) ->
    undefined;
get_cpp_default(#cpp_class_options{default = Default}) ->
    Default.
get_cpp_make_intermediate(undefined = _CppOptions) ->
    undefined;
get_cpp_make_intermediate(#cpp_class_options{make_intermediate = MakeIntermediate}) ->
    MakeIntermediate.

get_cpp_options(Model, Type) ->
    case ?model:get_type_info_silently_for_cpp_generator(Model, Type) of
    undefined ->
        undefined;
    #struct{cpp_options = CppOptions} ->
        CppOptions;
    #enum{cpp_options = CppOptions} ->
        CppOptions
    end.

is_shared_type(_Model, undefined) ->
    false;
is_shared_type(_Model, #enum{}) ->
    false;
is_shared_type(_Model, _TypeInfo) ->
    true.
is_explicitly_shared(Model, TypeInfo) ->
    is_shared_type(Model, TypeInfo).

is_by_ref(_Model, #union{}) ->
    true;
is_by_ref(Model, Type) ->
    is_by_ref_info(Model, ?model:get_type_info_silently_for_cpp_generator(Model, Type)).
is_by_ref_info(_Model, undefined) ->
    false;
is_by_ref_info(_Model, #enum{}) ->
    false;
is_by_ref_info(Model, TypeInfo) ->
    not(is_shared_type(Model, TypeInfo)).

is_nullable(Model, TypeInfo) ->
    is_shared_type(Model, TypeInfo).

%%% tests

get_model() ->
    ?model:create_for_cpp_generator(moodbox_model_definition:get_definition()).

generate_test() ->
    generate(get_model()).
