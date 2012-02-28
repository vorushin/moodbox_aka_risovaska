%%% System types - string, atom, integer, float, bool, datetime, binary


%%% records for model

-record(model, {
	  name,                                 % atom() - name of the model. unused
	  types_info,                           % [#struct{} | #enum{}] - info about all types, functions and enums
	  auth_free_functions,                  % [atom()] - list of all functions that does not require authentication
	  cpp_options                           % options for CPP code generation
	 }).

-record(struct,      {name,                     % struct_name() = type_name()
		      id,                       % integer() - id of struct (for binary serialization and cpp codegen)
                      fields,                   % [#field{}]
                      return_type,              % type() - type of return value of function (used in request manager)
		      is_result_for,            % is struct just a result-keeper for some call type
		      is_fault,                 % is struct a fault (exception) message
		      cpp_options               % options for CPP code generation
		     }).
-record(field,       {name,                     % atom()
		      id,                       % integer() - id of field (for binary serialization and cpp codegen)
                      type,                     % type()
                      is_optional = false,      % boolean()
		      default_value,            % term() = string() | integer() | atom() | float() | bool()
		      cpp_options               % options for CPP code generation
		     }).
-record(enum,        {name,                     % type_name()
		      id,                       % integer() - id of enum (for binary serialization and cpp codegen)
                      values,                   % [atom()] | [{atom(), integer()}]
		      cpp_options               % options for CPP code generation
		     }).
-record(union,       {types}).                  % [struct_name()]
-record(list,        {type}).                   % [type_name() | #union{} | #any{}]


%%% code generation

-record(cpp_model_options, {namespace_name, cpp_class_is_shared_default, cpp_class_is_shared_nullable_default}).
-record(cpp_class_options, {is_shared, is_nullable, replace_with, include_lib, default, skip, make_intermediate, cpp_return_type}).
-record(cpp_field_options, {cpp_type}).


%%% macroses

-define(Struct, moodbox_model:define_struct).
-define(Fault, moodbox_model:define_fault).
-define(Function, moodbox_model:define_function).
-define(Field, moodbox_model:define_field).
-define(FieldDefault, moodbox_model:define_field_default).
-define(FieldOptional, moodbox_model:define_field_optional).
-define(Enum, moodbox_model:define_enum).
-define(CppType(Type), #cpp_field_options{cpp_type = Type}).
