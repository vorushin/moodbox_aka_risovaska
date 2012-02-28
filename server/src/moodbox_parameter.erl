-module(moodbox_parameter).

%% External exports
-export([read_parameter/2]).

read_parameter(FileName, Parameter) ->
    case file:read_file(FileName) of
	{ok, Parameters} ->
	    StrParameters = binary_to_list(Parameters),
	    NamePos = string:str(StrParameters, Parameter ++ "="),
	    if 
		NamePos > 0 ->
		    Value = string:substr(StrParameters, NamePos + length(Parameter) + 1),
		    EndValuePos = string:str(Value, "\r\n"),
		    if 
			EndValuePos > 0 ->
			    string:substr(Value, 1, EndValuePos - 1);
			true ->
			    string:substr(Value, 1)
		    end;
		true ->
		    undefined	
	    end;
	{error, enoent} ->
	    case os:getenv(Parameter) of
		false ->
		    undefined;
		Value ->
		    Value
	    end;
	{error, Reason} ->
	    error_logger:error_report([{type, read_user_data_file}, {exception, Reason}, {trace, erlang:get_stacktrace()}]),
	    exit(Reason)
    end.
