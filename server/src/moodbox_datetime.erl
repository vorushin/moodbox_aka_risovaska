-module(moodbox_datetime).

-export([now/0]).
-export([add/2, date_diff/2]).
-export([microseconds/1, milliseconds/1, seconds/1, minutes/1, hours/1, days/1, weeks/1, months/1]).
-export([to_microseconds/1, to_milliseconds/1, to_seconds/1]).
-export([from_erl_now/1, to_erl_now/1, from_xsd/1, to_xsd/1, to_xsd/2, from_age/2, to_age/1, to_erlang_datetime/1, from_erlang_datetime/1]).

-define(ErlStartDateTime, {{1970,1,1},{0,0,0}}).

-define(datetime, moodbox_datetime).


now() ->
    from_erl_now(erlang:now()).

add(DateTime, TimeSpan) when is_integer(DateTime) andalso is_integer(TimeSpan) ->
    DateTime + TimeSpan.
date_diff(DateTime1, DateTime2) ->
    DateTime1 - DateTime2.

microseconds(MicroSeconds) ->
    MicroSeconds.
milliseconds(MilliSeconds) ->
    MilliSeconds * 1000.
seconds(Seconds) ->
    Seconds * 1000000.
minutes(Minutes) ->
    seconds(Minutes * 60).
hours(Hours) ->
    minutes(Hours * 60).
days(Days) ->
    hours(Days * 24).
weeks(Weeks) ->
    days(Weeks * 7).
months(Months) ->
    days(Months * 30).

to_microseconds(DateTime) ->
    DateTime.
to_milliseconds(DateTime) ->
    DateTime div 1000.
to_seconds(DateTime) ->
    DateTime div 1000000.
    
from_erl_now(DateTimeN) ->
    {MegaSec, Sec, MicroSec} = DateTimeN,
    MegaSec * 1000000000000 + Sec * 1000000 + MicroSec.


to_erl_now(DateTime) ->
    MicroSec = DateTime rem 1000000,
    Tmp = DateTime div 1000000,
    Sec = Tmp rem 1000000,
    MegaSec = Tmp div 1000000,

    {MegaSec, Sec, MicroSec}.


%% @note supports only full format, timezone must be UTC ('Z' at the end)
from_xsd(Str) ->
    [Y1, Y2, Y3, Y4, $-, M1, M2, $-, D1, D2, $T, H1, H2, $:, Mn1, Mn2, $:, S1, S2, $., Ms1, Ms2, Ms3, $Z] = Str,    

    Y = list_to_integer([Y1, Y2, Y3, Y4]),
    M = list_to_integer([M1, M2]),
    D = list_to_integer([D1, D2]),

    H = list_to_integer([H1, H2]),
    Mn = list_to_integer([Mn1, Mn2]),
    S = list_to_integer([S1, S2]),
    Ms = list_to_integer([Ms1, Ms2, Ms3]),

    Seconds = calendar:datetime_to_gregorian_seconds({{Y, M, D},{H, Mn, S}}) - calendar:datetime_to_gregorian_seconds(?ErlStartDateTime),

    (Seconds * 1000 + Ms) * 1000.


%% @todo more effective implementation
to_xsd(DateTime) ->
    DateTimeN = to_erl_now(DateTime),

    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_datetime(DateTimeN),
    {_, _, MicroSec} = DateTimeN,

    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ", [Year, Month, Day, Hour, Min, Sec, MicroSec div 1000])).
to_xsd(DateTime, TimeZone) ->
    to_xsd(DateTime) ++ TimeZone.

from_age(Age, IsPlusOneDay) ->
    {{Y, M, D}, {H, Mn, S}} = erlang:universaltime(),
    Seconds = calendar:datetime_to_gregorian_seconds({{Y - Age - 1, M, D},{H, Mn, S}}) - 
              calendar:datetime_to_gregorian_seconds(?ErlStartDateTime) + 
	      if IsPlusOneDay -> 86400; true -> 0 end,
    Seconds * 1000000.
to_age(BirthDay) ->
    {{Y, M, D}, {_, _, _}} = to_erlang_datetime(BirthDay),
    {{YNow, MNow, DNow}, {_, _, _}} = to_erlang_datetime(?datetime:now()),
    LastFullYear = if
		       MNow > M -> 1;
		       MNow < M  -> 0;
		       MNow == M ->
			   if
			       DNow >= D ->
				   1;
			       DNow < D -> 0
			   end
		   end,
    YNow - Y - 1 + LastFullYear.

to_erlang_datetime(DateTime) ->
    calendar:now_to_datetime(to_erl_now(DateTime)).
    
from_erlang_datetime(ErlangDateTime) ->
    Seconds = calendar:datetime_to_gregorian_seconds(ErlangDateTime) - calendar:datetime_to_gregorian_seconds(?ErlStartDateTime),
    Seconds * 1000000.
    
