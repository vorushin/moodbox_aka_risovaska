-module(moodbox_logger).

-export([init/0, logMessage/4, stop/0]).

init()->
    % @todo to add execution of add_report_handler here
    error_logger:tty(false),
    error_logger:logfile({open, "log.txt"}).

logMessage(Context, LogLevel, Category, Message) ->
    error_logger:error_report([{context, Context}, {logLevel, LogLevel}, {category, Category}, {message, Message}]).

stop()->
    % @todo to add execution of delete_report_handler here
    error_logger:logfile(close),
    error_logger:tty(true).
