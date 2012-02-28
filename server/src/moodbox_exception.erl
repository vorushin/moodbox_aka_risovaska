-module(moodbox_exception).

-export([create/2, create/4, create/5, is_exception/1, get_code/1, get_message/1]).

-include("moodbox_exception.hrl").


create(HandlingModule, ExceptionData) ->
    #?exception{handling_module = HandlingModule, exception_data = ExceptionData}.
create(HandlingModule, ExceptionData, InnerExceptionType, InnerException) ->
    #?exception{handling_module = HandlingModule, exception_data = ExceptionData, inner_exception = InnerException, inner_exception_type = InnerExceptionType}.
create(HandlingModule, ExceptionData, InnerExceptionType, InnerException, InnerExceptionStackTrace) ->
    #?exception{handling_module = HandlingModule, exception_data = ExceptionData, inner_exception = InnerException, inner_exception_type = InnerExceptionType, inner_exception_stack_trace = InnerExceptionStackTrace}.

is_exception(#?exception{}) ->
    true;
is_exception(_Something) ->
    false.

get_code(#?exception{handling_module = HandlingModule, exception_data = ExceptionData}) when is_tuple(ExceptionData) ->
    {HandlingModule, element(1, ExceptionData)};
get_code(#?exception{handling_module = HandlingModule}) ->
    {HandlingModule, undefined}.

get_message(#?exception{handling_module = HandlingModule} = Exception) ->
    HandlingModule:get_exception_message(Exception).
