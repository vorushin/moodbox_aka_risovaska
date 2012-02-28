-module(moodbox_validation).

-include("moodbox_config.hrl").
-include("moodbox_facade.hrl").

-export([is_length_in_range/3, check_age/1, is_valid_login/1, is_reserved_login/1, is_valid_password/1, 
	 is_valid_email/1]).

-define(datetime, moodbox_datetime).


is_length_in_range(Value, _MinLength, _MaxLength) when Value == undefined ->
    false;
is_length_in_range(Value, MinLength, MaxLength) when is_list(Value) ->
    (is_integer(MinLength) =/= true orelse length(Value) >= MinLength) andalso (is_integer(MaxLength) =/= true orelse length(Value) =< MaxLength);
is_length_in_range(Value, MinLength, MaxLength) when is_binary(Value) ->
    (is_integer(MinLength) =/= true orelse byte_size(Value) >= MinLength) andalso 
	(is_integer(MaxLength) =/= true orelse byte_size(Value) =< MaxLength).

check_age(undefined = _BirthDay) ->
    ok;
check_age(BirthDay) ->
    Age = ?datetime:to_age(BirthDay),
    if
	Age >= 13 ->
	    ok;
	true ->
	    ?age_must_be_13_or_greater
    end.

is_valid_login(Login) ->
    {ok, LoginExp} = re:compile(?LOGIN_VALIDATION_REGEXP),
    IsLengthInRange = is_length_in_range(Login, ?LOGIN_MINLENGTH, ?LOGIN_MAXLENGTH),
    if 
	 IsLengthInRange ->
	    case re:run(Login, LoginExp) of
		{match, _Result} ->
		    true;
		nomatch ->
		    false
	    end;
	true ->
	    false	    
    end.

is_reserved_login(Login) ->
    {ok, LoginExp} = re:compile(?LOGIN_RESERVED_REGEXP),
    case re:run(Login, LoginExp) of
	{match, _Result} ->
	    true;
	nomatch ->
	    false
    end.

is_valid_password(Password) ->
    is_length_in_range(Password, ?PASSWORD_MINLENGTH, ?PASSWORD_MAXLENGTH).

is_valid_email(Email) ->
    {ok, EmailExp} = re:compile(?EMAIL_VALIDATION_REGEXP),
    IsLengthInRange = is_length_in_range(Email, 0, ?EMAIL_MAXLENGTH),
    if 
	 IsLengthInRange ->
	    case re:run(Email, EmailExp) of
		{match, _Result} ->
		    true;
		nomatch ->
		    false
	    end;
	true ->
	    false	    
    end.
