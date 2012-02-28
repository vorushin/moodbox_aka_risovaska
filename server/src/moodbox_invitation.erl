-module(moodbox_invitation).

-export([create_prefixes/1, check_invitation/1, accept_invitation/1, get_statistics/0]).

-include("moodbox_config.hrl").
-include("moodbox_facade.hrl").
-include("moodbox_database.hrl").

-include_lib("stdlib/include/qlc.hrl").

-define(datetime, moodbox_datetime).
-define(TABLE_ROW, "<tr><td>~p</td><td>~p</td><td>~p</td></tr>").
-define(INVITATION_STATISTICS_TEMPLATE, "priv/templates/invitation_statistics_template.html").

-define(DEFAULT_PREFIX, "Default").
-define(DEFAULT_EMAIL_DOMAIN, "@example.com").

%invitation code status
-define(activated, activated).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API functions

create_prefixes([]) ->
    ok;
create_prefixes([{Prefix, Count} | RestPrefixes]) ->
    F = fun() ->
		case mnesia:read(invitation_prefix, Prefix, read) of
		    [] ->
			mnesia:write(#invitation_prefix{prefix = Prefix, count = Count});
		    _Other ->
			io:format("Prefix ~s already exists!", [Prefix])
		end
	end,
    mnesia:activity(transaction, F, [], mnesia_frag),
    create_prefixes(RestPrefixes).

check_invitation(InvitationCode) ->
    F = fun() ->
		case mnesia:index_read(invitation, InvitationCode, #invitation.code) of
		    [] ->
			?invalid_invite_code;
		    [Invitation] ->
			if
			    Invitation#invitation.state == undefined ->
				ok;
			    true ->
				?invalid_invite_code
			end
		end
    end,
    if
	InvitationCode == "" orelse InvitationCode == undefined ->
	    ?invalid_invite_code;
	true ->
	    mnesia:activity(async_dirty, F, [], mnesia_frag)
    end.

accept_invitation(InvitationCode) ->
    if
	InvitationCode == "" orelse InvitationCode == undefined ->
	    ?invalid_invite_code;
	true ->
	    case mnesia:index_read(invitation, InvitationCode, #invitation.code) of
		[] ->
		    ?invalid_invite_code;
		[Invitation] ->
		    if
			Invitation#invitation.state == undefined ->
			    mnesia:write(Invitation#invitation{state = ?activated, used_date = ?datetime:now()}),
			    ok;
			true ->
			    ?invalid_invite_code
		    end
	    end
    end.

get_statistics() ->
    F = fun() ->
		Invitations = qlc:e(qlc:keysort(#invitation.prefix,
 				  qlc:q([I || I <- mnesia:table(invitation)]),
				{order, descending})),
		TableData = get_statistics_internal(Invitations, "", 0, 0, ""),
		{ok, Template} = file:read_file(?INVITATION_STATISTICS_TEMPLATE),
		io_lib:format(Template, [TableData])
	end,
    mnesia:activity(async_dirty, F, [], mnesia_frag).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions

get_statistics_internal([], LastPrefix, PrefixAmount, UsedPrefixAmount, TableData) ->
    lists:flatten(io_lib:format(?TABLE_ROW, [LastPrefix, PrefixAmount, UsedPrefixAmount])) ++ TableData;
get_statistics_internal([Invitation | Rest], PreviousPrefix, PrefixAmount, UsedPrefixAmount, TableData) ->
    if 
	Invitation#invitation.prefix == PreviousPrefix ->
	    NewPrefixAmount = PrefixAmount + 1,
	    NewTableData = TableData,
	    if 
		Invitation#invitation.state == ?activated ->
		    NewUsedPrefixAmount = UsedPrefixAmount + 1;
		true ->
		    NewUsedPrefixAmount = UsedPrefixAmount
	    end;
	true ->
	    if 
		PreviousPrefix =/= "" ->
		    NewTableData  = lists:flatten(io_lib:format(?TABLE_ROW, [PreviousPrefix, PrefixAmount, UsedPrefixAmount])) ++ TableData;
		true ->
		    NewTableData = TableData
	    end,
	    NewPrefixAmount = 1,
	    if 
		Invitation#invitation.state == ?activated ->
		    NewUsedPrefixAmount = 1;
		true ->
		    NewUsedPrefixAmount = 0
	    end
    end,
    get_statistics_internal(Rest, Invitation#invitation.prefix, NewPrefixAmount, NewUsedPrefixAmount, NewTableData).
