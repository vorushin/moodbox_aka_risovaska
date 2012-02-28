-module(moodbox_string).

-export([is_empty/1, to_upper/1, len/1]).


is_empty(String) when String == "" orelse String == undefined ->
    true;
is_empty(_String) ->
    false.

to_upper(String) ->
    case os:type() of
	{win32, nt} ->
	    string:to_upper(String);
	_OtherType ->
	    %% faster varient without normalization:
            %% S = list_to_binary(xmerl_ucs:to_utf16be(String)),
            S = ustring:new(unicode, String),
	    ustring:pr(ustring:to_upper(S))
    end.

len(S) when is_list(S) ->
  length(S);
len(_S) ->
  0.
