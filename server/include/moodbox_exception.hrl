-record(moodbox_exception, {handling_module, exception_data, additional_data, inner_exception_type, inner_exception, inner_exception_stack_trace}).

-define(exception, moodbox_exception).
-define(exception_data(ExceptionData), #?exception{handling_module = ?MODULE, exception_data = ExceptionData}).
-define(exception_data_external(HandlingModule, ExceptionData), #?exception{handling_module = HandlingModule, exception_data = ExceptionData}).

-define(error(ExceptionData), throw(?exception:create(?MODULE, ExceptionData))).
-define(error_ext(Module, ExceptionData), throw(?exception:create(Module, ExceptionData))).
-define(error_inner(ExceptionData, InnerExceptionType, InnerException), throw(?exception:create(?MODULE, ExceptionData, InnerExceptionType, InnerException, erlang:get_stacktrace()))).
