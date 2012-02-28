-module(moodbox_sequence).

-export([sequence/1, sequence/2, create_sequence/1, init_sequence/2]).

%% sequence table record
-record(sequence, {key, index}).

%% Creates sequence table. Performed once
create_sequence(Nodes) ->
    mnesia:create_table(sequence,
        [{disc_copies, Nodes},
	 {frag_properties, [{node_pool, Nodes}, {n_disc_copies, 1}]},
         {attributes, record_info(fields, sequence)}]).

%% Inits or resets a sequence to Value
init_sequence(Name, Value) ->
     {atomic, ok} =
	mnesia:transaction(fun() ->
				   mnesia:write(#sequence{key=Name, index=Value})
			   end),
     ok.

%% Returns current value for sequence Name and increments
%% Sequence is created if not exists, and initial value 0 is returned.
sequence(Name) ->
     sequence(Name, 1).
%% increment sequence with Inc
sequence(Name, Inc) ->
     mnesia:dirty_update_counter(sequence, Name, Inc).

