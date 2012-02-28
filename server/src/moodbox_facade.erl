-module(moodbox_facade).

-export([get_server_info/1, get_auth_ticket/3, create_account/3, update_account/5, update_password/3, 
	 get_user_picture/3, get_contacts/1, get_contact/2, get_status/2, get_authorization/2, get_contact_logins/3, get_user_info/2, 
	 get_user_info_by_login/2, get_my_account/1, process_authorization_request/3, process_authorization_request_by_login/3, 
	 process_authorization_response/3, remove_from_contacts/2, send_friend_message/4, send_private_message/4,
	 get_next_artmessage_and_report/2, block_contact/2, unblock_contact/2, simple_search_contacts/4, 
	 advanced_search_contacts/9, get_published_images/3, get_sorted_users/5, reset_password/2, get_recovered_password/3, 
	 unsubscribe/3, send_email/3]).
-export([delete_images_and_moodstrip/2, get_random_bytes/2, send_news_to_all_subscribers/0]).
% command functions
-export([get_commands/2]).
% moodstrip functions
-export([create_moodstrip/4, add_picture_to_moodstrip/7, delete_moodstrip/2, get_moodstrip/3,
	 search_moodstrip/4, get_last_moodstrips/2, get_sorted_moodstrips/5, get_contact_moodstrips/5, 
	 get_contact_moodstrips_4rss/4]).
% notification functions
-export([notification_register/1, notification_unregister/2, notification_unregister/3, get_notifications/3, get_notification_timeout/1, 
	 notify/3, notify_by_user/2]).
% channel functions
-export([search_channel/4, get_channel_info/2, add_user_to_channel/2, delete_user_from_channel/2, get_next_channel_message/4, 
	 get_next_channel_message_url/4, send_channel_message/4, notify_by_channel/2, update_channel/6, obscene_channel_message/3, 
	 delete_message/4]).
% server functions
-export([get_srv_address/1]).
% invitation functions
-export([check_invitation/2]).

-include("moodbox_facade.hrl").
-include("moodbox_config.hrl").
-include("moodbox_database.hrl").
-include("moodbox_context.hrl").
-include("moodbox_repository.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(datetime, moodbox_datetime).
-define(auth, moodbox_auth).
-define(validation, moodbox_validation).
-define(database, moodbox_database).
-define(repository, moodbox_repository).

-define(safe_try(Expr), try Expr catch _:_ -> undefined end).

%%% exceptions
-export([get_exception_message/1]).
-include("moodbox_exception.hrl").
-record(invalid_user_id, {user_id}).
-record(update_user_picture_exception, {user_id, exception_message}).
-record(get_user_picture_exception, {user_id, exception_message}).
-record(invalid_notification_message_format, {message}).
-record(notification_session_not_found, {user_id}).
-record(add_image_to_moodstrip_exception, {user_id, moodstrip_id, exception_message}).
-record(send_message_exception, {user_id, exception_message}).
-record(publish_image_exception, {user_id, message_id, exception_message}).

-define(IS_INVITATION_REQUIRED, false).

get_server_info(_Context) ->
    #server_info{is_invitation_required = ?IS_INVITATION_REQUIRED, max_request_size = ?MAX_REQUEST_SIZE}.

get_auth_ticket(_Context, Login, Password) ->
    %io:format("Login: ~w, Password: ~w~n", [Login, Password]),
    CurrentDate = ?datetime:now(),
    case ?database:logon(Login, Password, CurrentDate, ?MAX_FAILED_LOGON_ATTEMPS_COUNT, ?FAILED_ATTEMPS_INTERVAL, ?LOCK_INTERVAL) of
	{ok = ResultCode, UserId} ->
	    #auth_ticket_result{result_code = ResultCode, auth_ticket = ?auth:get_encoded_ticket(UserId), user_id = UserId, 
				lifetime = ?datetime:to_seconds(?AUTH_TICKET_TIME_TO_LIVE)};
	invalid_credentials = ResultCode ->
	    #auth_ticket_result{result_code = ResultCode};
	{locked_too_many_attempts = ResultCode, LockTime} ->
	    #auth_ticket_result{result_code = ResultCode, lock_time = ?datetime:to_seconds(LockTime)}
    end.
    
create_account(Context, ContactFullInfo, _InviteCode) ->
    if
	Context#context.user_id =/= undefined ->
	    ?error(#invalid_user_id{user_id = Context#context.user_id});
	true ->
	    case ?validation:check_age(ContactFullInfo#user_account.birth_day) of
		Error when Error =/= ok ->
		    Error;
		ok ->
		    case ?validation:is_valid_login(ContactFullInfo#user_account.login) of
			false ->
			    ?invalid_login;
			true ->
			    case ?validation:is_reserved_login(ContactFullInfo#user_account.login) of
				true ->
				    ?login_is_not_available;
				false ->
				    case ?validation:is_valid_password(ContactFullInfo#user_account.password) of
					false ->
					    ?invalid_password;
					true ->
					    case ?validation:is_valid_email(ContactFullInfo#user_account.email) of
						false ->
						    ?invalid_email;
						true ->
						    ?database:update_account(ContactFullInfo, Context#context.language)
					    end
				    end
			    end
		    end
	    end
    end.
    
update_account(Context, ContactFullInfo, ThereIsUserPicture, UserPictureData, ContentType) ->
    if 
	Context#context.user_id =/= ContactFullInfo#user_account.id ->
	    ?error(#invalid_user_id{user_id = ContactFullInfo#user_account.id});
	true ->
	   ok
    end,
    case ?validation:check_age(ContactFullInfo#user_account.birth_day) of
	Error when Error =/= ok ->
	    Error;
	ok ->
	    case ?validation:is_valid_email(ContactFullInfo#user_account.email) of
		false ->
		    ?invalid_email;
		true ->
		    case ?database:update_account(ContactFullInfo, Context#context.language) of
			{ok, ?notify} ->
			    spawn(?MODULE, notify_by_user, [Context#context.user_id, ?contact_changed]);
			ok ->
			    ok
		    end,
		    if
			ThereIsUserPicture ->
			    update_user_picture(Context, UserPictureData, ContentType);
			true ->
			    ok
		    end
	    end
    end.

update_user_picture(Context, Data, ContentType) ->
    if 	
	size(Data) > ?USERPIC_MAX_SIZE ->
	    ?error(#update_user_picture_exception{user_id = Context#context.user_id, exception_message = ?userpic_size_too_big});
	true ->
	    if 
		Data == undefined -> 
		    case ?repository:delete_file(?USERPIC_PREFIX, integer_to_list(Context#context.user_id)) of
			ok ->
			    ok;
			{error, Error} ->
			    ?error(#update_user_picture_exception{user_id = Context#context.user_id, exception_message = Error})
		    end;
		true ->
		    % by default: ContentType = "image/png"
		    case ?repository:put_object(?USERPIC_PREFIX, Context#context.user_id, Data, ContentType, [], true) of 
			ok ->
			    ok;
			{error, Error} ->
			    ?error(#update_user_picture_exception{user_id = Context#context.user_id, exception_message = Error})
		    end
	    end
    end,
    % temporary switched off because client doesn't listen this notification now
    %spawn(?MODULE, notify_by_user, [Context#context.user_id, ?userpic_changed]),
    ok.

get_user_picture(_Context, UserId, LastChangedDate) -> 
    if  
	LastChangedDate == undefined ->
	    NewDate = undefined;                       
	LastChangedDate > 0 andalso LastChangedDate < 2100000000000000 -> % because httpd_util:rfc1123_date crashes if datetime is higher
	    NewDate = ?datetime:to_erlang_datetime(LastChangedDate);
	true -> 
	    NewDate = undefined
    end,
    case ?repository:get_file_with_header(?USERPIC_PREFIX, integer_to_list(UserId), NewDate) of
	{not_modified, _Headers} ->
	    #user_picture_result{result_code = ?not_modified, user_id = UserId};
	{ok, Headers, Data} ->
	    case lists:keytake(?S3_LAST_MODIFIED_HEADER, 1, Headers) of
		{value, {_, DateTime}, _} -> 
		    #user_picture_result{result_code = ok, user_id = UserId, 
					 last_change_date = ?datetime:from_erlang_datetime(httpd_util:convert_request_date(DateTime)), 
					 picture_data = Data};
		_ -> 
		    #user_picture_result{result_code = ok, user_id = UserId, last_change_date = undefined, picture_data = Data}
	    end;
	{error, {"NoSuchKey", _Description, _RequestId}} ->
	    #user_picture_result{result_code = ?not_found, user_id = UserId};
	{error, Error} ->
	    ?error(#get_user_picture_exception{user_id = UserId, exception_message = Error})
    end.

update_password(Context, NewPassword, CurrentPassword) ->
    case ?validation:is_valid_password(NewPassword) of
	false ->
	    ?invalid_password;
	true ->
	    ?database:update_password(Context#context.user_id, NewPassword, CurrentPassword)
    end.

get_contact(Context, ContactUserId) ->
    [ContactInfo] = ?database:get_contacts(Context#context.user_id, ContactUserId),
    ContactInfo.

get_contacts(Context) ->
    lists:filter(fun(X) -> if 
			       X == undefined ->
				  false;
			       true ->
				   true
			   end
		 end, ?database:get_contacts(Context#context.user_id)).

get_status(Context, ContactUserId) ->
    ?database:get_status(Context#context.user_id, ContactUserId).

get_authorization(Context, ContactUserId) ->
    ?database:get_authorization(Context#context.user_id, ContactUserId).

get_contact_logins(_Context, ContactType, ContactId) ->
    ?database:get_contact_logins(ContactType, ContactId).

get_my_account(Context) ->
    ?database:get_my_account(Context#context.user_id, Context#context.language).

get_user_info(_Context, UserId) ->
    ?database:get_user_info(UserId).

get_user_info_by_login(_Context, Login) ->
    ?database:get_user_info_by_login(Login).

process_authorization_request(Context, RecipientId, AuthorizationMessage) ->
    if
	Context#context.user_id == RecipientId ->
	    ?error(#cannot_authorize_yourself_exception{user_id = RecipientId});
	true ->
	    ok
    end,
    Result = ?database:process_authorization_request(Context#context.user_id, RecipientId, AuthorizationMessage, ?MAX_CONTACTS_COUNT),
    if
	Result == ok orelse Result == ?authorized_by_this_request orelse Result == ?already_authorized ->
	    % notification
	    spawn(?MODULE, notify, [Context#context.user_id, RecipientId, ?authorization_changed]),
	    Result;
	Result == ?user_blocked_you ->
	    ok;
	true ->
	    Result
    end.

process_authorization_request_by_login(Context, RecipientLogin, AuthorizationMessage) ->
    {RecipientId, Result} = ?database:process_authorization_request_by_login(Context#context.user_id, RecipientLogin, AuthorizationMessage, 
									     ?MAX_CONTACTS_COUNT),
    if
	Result == ok orelse Result == ?authorized_by_this_request orelse Result == ?already_authorized ->
	    % notification
	    spawn(?MODULE, notify, [Context#context.user_id, RecipientId, ?authorization_changed]),
	    Result;
	Result == ?user_blocked_you ->
	    ok;
	true ->
	    Result
    end.

process_authorization_response(Context, RecipientId, IsAccepted) ->
    if
	Context#context.user_id == RecipientId ->
	    ?error(#cannot_authorize_yourself_exception{user_id = RecipientId});
	true -> 
	    ok
    end,
    case ?database:process_authorization_response(Context#context.user_id, RecipientId, IsAccepted) of
	ok ->
	    %notification
	    spawn(?MODULE, notify, [Context#context.user_id, RecipientId, ?authorization_changed]),
	    ok;
	?authorization_request_declined ->
	    ok;
	OtherResult ->
	    OtherResult
    end.

remove_from_contacts(Context, ContactUserId) ->
    case ?database:remove_from_contacts(Context#context.user_id, ContactUserId) of
	ok ->
	    % notification
	    spawn(?MODULE, notify, [Context#context.user_id, ContactUserId, ?authorization_changed]),
	    ?database:skip_artmessages(Context#context.user_id, ContactUserId);
	Result ->
	    Result
    end.

send_friend_message(Context, IsPublic, Message, MetaData) ->
    SendDate = ?datetime:now(),
    MessageId = ?database:send_friend_message(Context#context.user_id, IsPublic, SendDate),
    if 
	is_list(MetaData) == false ->
	    NewMetaData = "";
	true ->
	    NewMetaData = MetaData
    end,
    case ?repository:put_object(?ARTMESSAGE_PREFIX, MessageId, Message, get_content_type(NewMetaData),
				 if IsPublic -> []; true -> [{"data", NewMetaData}] end, 
				 IsPublic) of
	ok ->
	    ?database:delivery_artmessage(MessageId, Context#context.user_id, ?friends);
	Error ->
	    ?error(#send_message_exception{user_id = Context#context.user_id, exception_message = Error})
    end,
    % notification
    spawn(?MODULE, notify_by_user, [Context#context.user_id, ?new_message]),
    #send_message_result{result_code = ok, message_id = MessageId, send_date = SendDate}.

send_private_message(Context, RecipientId, Message, MetaData) ->
    SendDate = ?datetime:now(),
    case ?database:send_private_message(Context#context.user_id, RecipientId, SendDate) of
	{ok, MessageId} ->
	    if 
		is_list(MetaData) == false ->
		    NewMetaData = "";
		true ->
		    NewMetaData = MetaData
	    end,
	    case ?repository:put_object(?ARTMESSAGE_PREFIX, MessageId, Message, get_content_type(NewMetaData),
					[{"data", NewMetaData}],
					false) of
		ok ->
		    ?database:delivery_artmessage(MessageId, Context#context.user_id, ?private);
		Error ->
		    ?error(#send_message_exception{user_id = Context#context.user_id, exception_message = Error})
	    end,
	    % notification
	    spawn(?MODULE, notify, [Context#context.user_id, RecipientId, ?new_message]),
	    #send_message_result{result_code = ok, message_id = MessageId, send_date = SendDate};
	OtherResult ->
	    #send_message_result{result_code = OtherResult}
    end.
	
get_next_artmessage_and_report(Context, PreviousMessageId) ->
    case ?database:get_next_artmessage_and_report(Context#context.user_id, PreviousMessageId) of
	undefined ->
	    undefined;
	{true, QueueItem} ->
	    #artmessage_result{result_code = ok, message_id = QueueItem#queue_item.message_id, type = QueueItem#queue_item.type,
					       author_id = QueueItem#queue_item.author_id, send_date = QueueItem#queue_item.send_date, 
					       url = ?repository:get_url(?ARTMESSAGE_PREFIX, QueueItem#queue_item.message_id)};
	{false, QueueItem} ->
	    case ?repository:get_file_with_header(?ARTMESSAGE_PREFIX, integer_to_list(QueueItem#queue_item.message_id)) of
		{error, {"NoSuchKey", _Description, _RequestId}} ->
		    #artmessage_result{result_code = ?message_data_not_found, message_id = QueueItem#queue_item.message_id,
				       type = QueueItem#queue_item.type, author_id = QueueItem#queue_item.author_id, 
				       send_date = QueueItem#queue_item.send_date};
		{ok, Headers, Data} ->
		    case lists:keytake(?S3_USER_METADATA_HEADER, 1, Headers) of
			{value, {_, MetaData}, _} -> 
			    #artmessage_result{result_code = ok, message_id = QueueItem#queue_item.message_id, type = QueueItem#queue_item.type,
					       author_id = QueueItem#queue_item.author_id, send_date = QueueItem#queue_item.send_date, 
					       data = Data, metadata = MetaData};
			_ ->
			    #artmessage_result{result_code = ok, message_id = QueueItem#queue_item.message_id, type = QueueItem#queue_item.type,
					       author_id = QueueItem#queue_item.author_id, send_date = QueueItem#queue_item.send_date, 
					       data = Data}
		    end
	    end
    end.

block_contact(Context, ContactUserId) ->
    ?database:block_contact(Context#context.user_id, ContactUserId).

unblock_contact(Context, ContactUserId) ->
    ?database:unblock_contact(Context#context.user_id, ContactUserId).

simple_search_contacts(_Context, PageNumber, RecordsPerPage, Value) ->
    {NewPageNumber, NewRecordsPerPage} = check_page_number_and_count_of_records(PageNumber, RecordsPerPage),
    ?database:simple_search_contacts(NewPageNumber, NewRecordsPerPage, Value).

advanced_search_contacts(_Context, PageNumber, RecordsPerPage, Value, Country, City, Sex, AgeMin, AgeMax) ->
    {NewPageNumber, NewRecordsPerPage} = check_page_number_and_count_of_records(PageNumber, RecordsPerPage),
    ?database:advanced_search_contacts(NewPageNumber, NewRecordsPerPage, Value, Country, City, Sex, AgeMin, AgeMax).

create_moodstrip(Context, Title, IsHidden, ChannelId) ->
    SendDate = ?datetime:now(),
    ?database:create_moodstrip(Context#context.user_id, SendDate, Title, Context#context.language, IsHidden, ChannelId).

add_picture_to_moodstrip(Context, MoodStripId, MessageId, Author, PictureData, ContentType, IsLastImage) ->
    SendDate = ?datetime:now(),
    SortedTable = get_sorted_table(Context#context.language),
    case ?database:add_image_to_moodstrip(MoodStripId, MessageId, Context#context.user_id, Author, SendDate, IsLastImage, SortedTable) of
	{ok, ImageId, Moodstrip} ->
	    case ?repository:put_object(?MOODSTRIP_PREFIX, ImageId, PictureData, ContentType,
	    				[{"moodstrip_id", integer_to_list(MoodStripId)}],
	    				true) of
		ok ->
		    if
			IsLastImage ->
			    {Lj, LjCut} = create_lj_post(lists:reverse(Moodstrip#moodstrip.items), length(Moodstrip#moodstrip.items), 1, [], []),
			    case Context#context.language of
				'ru' ->
				    Urls = [#publishing_way{code = ?simple_url, url = io_lib:format(?URL_WEB_RU, [MoodStripId])},
					    #publishing_way{code = ?embedded_flash, url = io_lib:format(?URL_WIDGET_RU, [MoodStripId, MoodStripId])},
					    #publishing_way{code = ?lj, url = Lj ++ io_lib:format(?LJ_POST_SIGNATURE_START_RU, [MoodStripId]) ++ 
							    Moodstrip#moodstrip.title ++ ?LJ_POST_SIGNATURE_END_RU},
					    #publishing_way{code = ?lj_cut, url = LjCut ++ io_lib:format(?LJ_POST_SIGNATURE_START_RU, [MoodStripId]) ++ 
							    Moodstrip#moodstrip.title ++ ?LJ_POST_SIGNATURE_END_RU}
					   ];
				_ ->
				    Urls = [#publishing_way{code = ?simple_url, url = io_lib:format(?URL_WEB, [MoodStripId])},
					    #publishing_way{code = ?embedded_flash, url = io_lib:format(?URL_WIDGET, [MoodStripId, MoodStripId])},
					    #publishing_way{code = ?lj, url = Lj ++ io_lib:format(?LJ_POST_SIGNATURE_START, [MoodStripId]) ++ 
							    Moodstrip#moodstrip.title ++ ?LJ_POST_SIGNATURE_END},
					    #publishing_way{code = ?lj_cut, url = LjCut ++ io_lib:format(?LJ_POST_SIGNATURE_START, [MoodStripId]) ++ 
							    Moodstrip#moodstrip.title ++ ?LJ_POST_SIGNATURE_END}
					   ]
			    end;
			true ->
			    Urls = undefined
		    end,
		    #publishing_moodstrip_result{result_code = ok,
						 urls = Urls};
		Error ->
		    ?error(#add_image_to_moodstrip_exception{user_id = Context#context.user_id, moodstrip_id = MoodStripId, exception_message = Error})
	    end;
	Result ->
	    #publishing_moodstrip_result{result_code = Result}
    end.

create_lj_post([], Count, Index, Lj, LjCut) ->
    {lists:flatten(Lj), lists:flatten(LjCut ++ if Index > 3 andalso Count > 3  -> "</lj-cut>"; true -> "" end)};
create_lj_post([Item | RestItems], Count, Index, Lj, LjCut) ->
    LjItem = io_lib:format(?LJ_URL_ITEM, [?repository:get_url(?MOODSTRIP_PREFIX, Item#moodstrip_item.image_id)]),
    create_lj_post(RestItems, Count, Index + 1, Lj ++ LjItem, LjCut ++ if Index == 3 andalso Count > 3 -> "<lj-cut>"; true -> "" end ++ LjItem).

delete_moodstrip(Context, MoodStripId) ->
    case ?database:mark_moodstrip_for_deleting(Context#context.user_id, MoodStripId) of
	{ok, MoodStripImages} ->
	    % delete images from S3 and moodstrip from DB
	    spawn(?MODULE, delete_images_and_moodstrip, [MoodStripId, MoodStripImages]),
	    ok;
	?moodstrip_not_found ->
	    ?moodstrip_not_found
    end.

get_moodstrip(_Context, MoodStripId, IsPreview) ->
    ?database:get_moodstrip(MoodStripId, IsPreview).

search_moodstrip(Context, PageNumber, RecordsPerPage, Title) ->
    {NewPageNumber, NewRecordsPerPage} = check_page_number_and_count_of_records(PageNumber, RecordsPerPage),
    ?database:search_moodstrip(NewPageNumber, NewRecordsPerPage, Title, Context#context.language).

get_last_moodstrips(Context, Count) ->
    NewCount = if 
		  Count > ?MAX_LAST_MOODSTRIPS orelse Count < 1 ->
		      ?MAX_LAST_MOODSTRIPS;
		  true ->
		      Count
	      end,
    DateTimeMin = get_now_by_date_range(?month),
    SortedTable = get_sorted_table(Context#context.language),
    ?database:get_last_moodstrips(NewCount, DateTimeMin, SortedTable).

get_sorted_moodstrips(Context, PageNumber, RecordsPerPage, SortedBy, DateRange) ->
    {NewPageNumber, NewRecordsPerPage} = check_page_number_and_count_of_records(PageNumber, RecordsPerPage),
    DateTimeMin = get_now_by_date_range(DateRange),
    SortedTable = get_sorted_table(Context#context.language),
    ?database:get_sorted_moodstrips(NewPageNumber, NewRecordsPerPage, SortedBy, DateTimeMin, SortedTable).

get_contact_moodstrips(_Context, PageNumber, RecordsPerPage, ContactType, ContactId) ->
    {NewPageNumber, NewRecordsPerPage} = check_page_number_and_count_of_records(PageNumber, RecordsPerPage),
    ?database:get_contact_moodstrips(NewPageNumber, NewRecordsPerPage, ContactType, ContactId).

get_contact_moodstrips_4rss(_Context, ContactType, ContactId, Count) ->
    if 
	Count < 1 orelse Count > 50 ->
          NewCount = 10;
	true ->
          NewCount = Count
    end,
    ?database:get_contact_moodstrips_4rss(ContactType, ContactId, NewCount).

get_sorted_users(_Context, PageNumber, RecordsPerPage, SortedBy, DateRange) ->
    {NewPageNumber, NewRecordsPerPage} = check_page_number_and_count_of_records(PageNumber, RecordsPerPage),
    DateTimeMin = get_now_by_date_range(DateRange),
    ?database:get_sorted_users(NewPageNumber, NewRecordsPerPage, SortedBy, DateTimeMin).

reset_password(Context, Login) ->
    ResetCode = get_random_bytes(?RESET_CODE_LENGTH, []),
    case ?database:reset_password(Login, ResetCode, get_random_bytes(?AUTO_GENERATE_PASSWORD_LENGTH, [])) of
	{ok, UserId, _Name, Email} ->
	    if
		Context#context.language == 'ru' ->
		    Subj = "=?utf-8?B?" ++ base64:encode_to_string(?MAIL_PSWD_REMIND_HEADER_RU) ++ "?=",
		    Body = io_lib:format(?MAIL_PSWD_REMIND_BODY_RU, [ResetCode, UserId]);
		true ->
		    Subj = ?MAIL_PSWD_REMIND_HEADER,
		    Body = io_lib:format(?MAIL_PSWD_REMIND_BODY, [ResetCode, UserId])
	    end,
	    moodbox_email:send_mail(Email, Subj, Body),
	    ok;
	Result ->
	    Result
    end.

get_recovered_password(_Context, UserId, ResetCode) ->
    ?database:get_recovered_password(UserId, ResetCode).

unsubscribe(_Context, UserId, UnsubscribeCode) ->
    ?database:unsubscribe(UserId, UnsubscribeCode).

get_published_images(_Context, ContactType, ContactId) ->
    ?database:get_published_images(ContactType, ContactId).

send_email(Context, MoodstripID, Text) ->
    case ?database:send_email(Context#context.user_id, MoodstripID) of
	{ok, Email, SentUserLogin, MoodstripTitle} ->
	    if
		Context#context.language == 'ru' ->
		    Subj = "=?utf-8?B?" ++ base64:encode_to_string(?MAIL_MOODSTRIP_COMMENT_HEADER_RU) ++ "?=",
		    Body = io_lib:format(?MAIL_MOODSTRIP_COMMENT_BEGIN, [SentUserLogin]) ++ MoodstripTitle ++ ":\r\n\r\n" ++
			Text ++ "\r\n\r\n" ++ io_lib:format(?MAIL_MOODSTRIP_COMMENT_END_RU, [MoodstripID]);
		true ->
		    Subj = ?MAIL_MOODSTRIP_COMMENT_HEADER,
		    Body = io_lib:format(?MAIL_MOODSTRIP_COMMENT_BEGIN, [SentUserLogin]) ++ MoodstripTitle ++ ":\r\n\r\n" ++ 
			Text ++ "\r\n\r\n" ++ io_lib:format(?MAIL_MOODSTRIP_COMMENT_END, [MoodstripID])
	    end,
	    moodbox_email:send_mail(Email, Subj, Body),
	    ok;
	OtherResult ->
	    OtherResult
    end.

send_news_to_all_subscribers() ->
    %Result = ?database:get_subscribers(),
    io:format("Start sending letters...~n", []),
    {ok, B} = file:read_file("emails.txt"),
    %Subject = "=?utf-8?B?" ++ base64:encode_to_string(?MAIL_PSWD_ARTYTALK_HEADER) ++ "?=",
    %Message = ?MAIL_PSWD_ARTYTALK_BODY,
    Result = binary_to_list(B),
    send_news(Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command functions

get_commands(Context, PreviousCommandId) ->
    ?database:get_commands(Context#context.user_id, PreviousCommandId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% notification functions

notification_register(Context) ->
    Key = get_random_bytes(?NOTIFICATION_KEY_LENGTH, []),
    {PreviousSession, NotificationServer} = ?database:notification_register(Context#context.user_id, Key),
    % register user in the notification server
    case PreviousSession of
	% first connection to the system
	[] ->
	    moodbox_notification:add_user(NotificationServer#notification_server.pid, Context#context.user_id, Key);
	[Session] ->
	    if
	        % there is already connection on the same notification server
		Session#session.server_pid == NotificationServer#notification_server.pid -> 
		    moodbox_notification:replace_user(NotificationServer#notification_server.pid, Context#context.user_id, Key);
	        % there is already connection on a different notification server
		true ->
		    moodbox_notification:disconnect_user(Session#session.server_pid, Context#context.user_id),
		    moodbox_notification:add_user(NotificationServer#notification_server.pid, Context#context.user_id, Key)
	    end
    end,
    % notification
    spawn(?MODULE, notify_by_user, [Context#context.user_id, ?status_changed]),
    #notification_registration_result{server = io_lib:format(?SERVER_URL, [NotificationServer#notification_server.ip]), key = Key}.

notification_unregister(Context, Key) ->
    notification_unregister(Context, Key, false).

notification_unregister(Context, Key, Is_Internal) ->
    case ?database:notification_unregister(Context#context.user_id, Key) of
	?session_is_not_exist ->
	    ok;
	{?invalid_key, Session} ->
	    if
		Is_Internal ->
		    moodbox_notification:disconnect_user(Session#session.server_pid, Context#context.user_id),
		    ok;
		true ->
		    ok
	    end;		
	{ok, Session} ->
	    moodbox_notification:disconnect_user(Session#session.server_pid, Context#context.user_id),
	    % notification
	    spawn(?MODULE, notify_by_user, [Context#context.user_id, ?status_changed]),
	    ok
    end.

get_notifications(Context, Key, PacketId) ->
    case ?database:check_notification_key(Context#context.user_id, Key) of
	undefined ->
	    ?error(#notification_session_not_found{user_id = Context#context.user_id});
	?invalid_notification_key ->
	    #notification_result{notifications = [#notification{event = ?disconnect}]};
	{ok, ServerPID} ->
	    moodbox_notification:add_process(ServerPID, Context#context.user_id, Key, self(), PacketId),
	    receive 
		?create_packet ->
		    moodbox_notification:create_packet(Context#context.user_id);
		?retry_packet ->
		    moodbox_notification:retry_packet(Context#context.user_id);
	        ?disconnect ->
		    #notification_result{notifications = [#notification{event = ?disconnect}]}
	        after
		    ?datetime:to_milliseconds(?GET_NOTIFICATION_TIMEOUT) ->
			moodbox_notification:update_user_internal(Context#context.user_id), % update user's last modified date
			#notification_result{}
	    end
    end.

get_notification_timeout(_Context) ->
    ?datetime:to_seconds(?GET_NOTIFICATION_TIMEOUT).

notify(FromUserId, ForUserId, Event) ->
    ?database:notify(FromUserId, ForUserId, Event).

notify_by_user(FromUserId, Event) ->
    ?database:notify_by_user(FromUserId, Event).

notify_by_channel(ChannelId, Event) ->
    ?database:notify_by_channel(ChannelId, Event, undefined).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% channel functions

search_channel(Context, PageNumber, RecordsPerPage, Value) ->
    ?database:search_channel(PageNumber, RecordsPerPage, Context#context.language, Value).

get_channel_info(_Context, ChannelId) ->
    ?database:get_channel_info(ChannelId).

add_user_to_channel(Context, ChannelId) ->
    ?database:add_user_to_channel(ChannelId, Context#context.user_id).

delete_user_from_channel(Context, ChannelId) ->
    ?database:delete_user_from_channel(ChannelId, Context#context.user_id).

get_next_channel_message(_Context, ChannelId, LastMessageId, SkipLastMessage) ->
    case ?database:get_next_channel_message(ChannelId, LastMessageId, SkipLastMessage) of
	undefined ->
	    undefined;
	ChannelMessageItem ->
	    case ?repository:get_file_with_header(?ARTMESSAGE_PREFIX, integer_to_list(ChannelMessageItem#channel_message_item.message_id)) of
			{error, {"NoSuchKey", _Description, _RequestId}} ->
				#channel_message_result{result_code = ?message_data_not_found, 
						    message_id = ChannelMessageItem#channel_message_item.message_id, 
						    author_id = ChannelMessageItem#channel_message_item.author_id,
						    author_login = ChannelMessageItem#channel_message_item.author_login,
					    	send_date = ChannelMessageItem#channel_message_item.send_date};
			{ok, Headers, Data} ->
				case lists:keytake(?S3_USER_METADATA_HEADER, 1, Headers) of
				{value, {_, MetaData}, _} ->
				    #channel_message_result{result_code = ok, message_id = ChannelMessageItem#channel_message_item.message_id,
							    author_id = ChannelMessageItem#channel_message_item.author_id,
							    author_login = ChannelMessageItem#channel_message_item.author_login,
							    send_date = ChannelMessageItem#channel_message_item.send_date,
							    data = Data, metadata = MetaData};
				_ -> 
				    #channel_message_result{result_code = ok, message_id = ChannelMessageItem#channel_message_item.message_id,
							    author_id = ChannelMessageItem#channel_message_item.author_id,
							    author_login = ChannelMessageItem#channel_message_item.author_login,
							    send_date = ChannelMessageItem#channel_message_item.send_date,
							    data = Data}
			    end
		    end
    end.

get_next_channel_message_url(_Context, ChannelId, LastMessageId, SkipLastMessage) ->
    case ?database:get_next_channel_message(ChannelId, LastMessageId, SkipLastMessage) of
	undefined ->
	    undefined;
	ChannelMessageItem ->
	    #channel_message_url{result_code = ok, 
				 message_id = ChannelMessageItem#channel_message_item.message_id,
				 author_id = ChannelMessageItem#channel_message_item.author_id,
				 author_login = ChannelMessageItem#channel_message_item.author_login,
				 send_date = ChannelMessageItem#channel_message_item.send_date,
				 url = ?repository:get_url(?ARTMESSAGE_PREFIX, ChannelMessageItem#channel_message_item.message_id)}
    end.

send_channel_message(Context, ChannelId, Message, MetaData) ->
    SendDate = ?datetime:now(),
    case ?database:send_channel_message(ChannelId, Context#context.user_id, SendDate) of
	{ok, MessageId} ->
	    if 
		is_list(MetaData) ->
		    NewMetaData = MetaData;
		true ->
		    NewMetaData = ""
	    end,
	    case ?repository:put_object(?ARTMESSAGE_PREFIX, MessageId, Message, get_content_type(NewMetaData),
					[], true) of % {"data", NewMetaData}
		ok ->
		    Result = ?database:delivery_channel_message(ChannelId, MessageId, Context#context.user_id, SendDate),
		    % notification
		    spawn(?MODULE, notify_by_channel, [ChannelId, ?new_channel_message]);
		Error ->
		    Result = undefined,
		    ?error(#send_message_exception{user_id = Context#context.user_id, exception_message = Error})
	    end;
	{error, ErrorResult} ->
	    MessageId = undefined,
	    Result = ErrorResult
    end,
    #send_message_result{result_code = Result, message_id = MessageId, send_date = SendDate}.

update_channel(Context, ChannelId, Title, ShortDescription, FullDescription, Order) ->
    ?database:update_channel(Context#context.user_id, ChannelId, Title, ShortDescription, FullDescription, Context#context.language, Order).

obscene_channel_message(Context, ChannelId, MessageId) ->
    ?database:obscene_channel_message(Context#context.user_id, ChannelId, MessageId).

delete_message(Context, ContactId, MessageType, MessageId) ->
    ?database:delete_message(Context#context.user_id, ContactId, MessageType, MessageId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% server functions

get_srv_address(_Context) ->
    atom_to_list(node()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% invitation functions

check_invitation(_Context, InvitationCode) ->
    moodbox_invitation:check_invitation(InvitationCode).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% internal functions

get_content_type(MetaData) ->
    StartPos = string:str(MetaData, "<Image Type=\""),
    if
	StartPos > 0 ->
	    Value = string:substr(MetaData, StartPos + 13, 11),
	    EndPos = string:chr(Value, 34), % 34 - quotation
	    if 
		EndPos > 0 ->
		    string:substr(Value, 1, EndPos - 1);
		true ->
		    "image/png"
	    end;
	true ->
	    "image/png"
    end.

get_sorted_table(Language) ->
    if 
	Language == undefined orelse Language == en ->
	    moodstrip_sorted;
	true ->
	    list_to_atom("moodstrip_sorted_" ++ atom_to_list(Language))
    end.

check_page_number_and_count_of_records(PageNumber, RecordsPerPage) ->
    if 
       PageNumber < 1 ->
	   NewPageNumber = 0;
       true ->
	   NewPageNumber = PageNumber - 1
    end,
    if 
       RecordsPerPage > ?MAX_RECORDS_PER_PAGE orelse RecordsPerPage < 1 ->
	   NewRecordsPerPage = ?MAX_RECORDS_PER_PAGE;
       true ->
	   NewRecordsPerPage = RecordsPerPage
   end,
   {NewPageNumber, NewRecordsPerPage}.

get_random_bytes(0, Bytes) ->
    Bytes;
get_random_bytes(Length, Bytes) ->
    N = case crypto:rand_uniform(65, 126) of % 'A' - '~'
	N1 when N1 > 90 andalso N1 < 97 -> % 
	    N1 - 43; % '0' - '5'
	N2 when N2 > 122 andalso N2 < 127 ->
	    N2 - 69; % '6' - '9'
	N3 ->
	    N3
    end,
    get_random_bytes(Length - 1, [N | Bytes]).

delete_images_and_moodstrip(MoodStripId, MoodStripImages) ->
    delete_moodstrip_images_from_repository(MoodStripImages),
    ?database:delete_moodstrip(MoodStripId).

delete_moodstrip_images_from_repository([]) ->
    ok;
delete_moodstrip_images_from_repository([Head | Rest]) ->
    ?repository:delete_file(?MOODSTRIP_PREFIX, integer_to_list(Head#moodstrip_item.image_id)),
    delete_moodstrip_images_from_repository(Rest).

get_now_by_date_range(DateRange) ->
   {{Y, M, D}, {H, Mn, S}} = erlang:localtime(),
      case DateRange of
       ?today ->
	   GregorianSeconds = calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H, Mn, S}}),
	   NewDate = calendar:gregorian_seconds_to_datetime(GregorianSeconds - 86400),
	   ?datetime:from_erlang_datetime(NewDate);
       ?week ->
	   GregorianDays = calendar:date_to_gregorian_days(Y, M, D),
	   NewDate = calendar:gregorian_days_to_date(GregorianDays - 7),
	   ?datetime:from_erlang_datetime({NewDate, {0, 0, 0}});
       ?month ->
	   GregorianDays = calendar:date_to_gregorian_days(Y, M, D),
	   NewDate = calendar:gregorian_days_to_date(GregorianDays - 30),
	   ?datetime:from_erlang_datetime({NewDate, {0, 0, 0}});
       ?all_time ->
	   0
   end.

send_news([]) ->
     io:format("Sending emails was finished", []);
send_news(Emails) ->
    Position = string:str(Emails, ", ") - 1,
    if 
	Position > 0 ->
	    {UserId, TempEmails} = lists:split(Position, Emails),
	    Position2 = string:str(TempEmails, "\r\n") - 1,
	    if 
		Position2 > 0 ->
		    {_Rubbish, TempEmails2} = lists:split(2, TempEmails),
		    {Email, TempEmails3} = lists:split(Position2 - 2, TempEmails2),
		    {_Rubbish2, RestEmails} = lists:split(2, TempEmails3),
		    try
			send_news_internal(list_to_integer(UserId), Email)
		    catch
			_:_ ->
			    error
		    end,
		    send_news(RestEmails);
		true ->
		    io:format("Sending emails was finished~n", [])
	    end;
	true ->
	    io:format("Sending emails was finished~n", [])
    end.

%send_news([]) ->
%    io:format("Sending emails was finished", []);
%send_news([{UserId, Email} | RestEmails]) ->
%     send_news_internal(UserId, Email),
%     send_news(RestEmails).

send_news_internal(UserId, Email) ->
    if
 	Email == [] orelse Email == undefined ->
 	    ok;
 	true ->
 	    UnsubscribeCode = url_encode(base64:encode_to_string(crypto:aes_cfb_128_encrypt(?AES_KEY, ?AES_VECTOR, 
											    term_to_binary([UserId, Email, ?SECRET_CODE])))),
 	    try
 		io:format("Sending a letter to: ~s...~n", [Email]),
 		Msg = email_msg:simp_msg(?MAIL_FROM, Email, "=?utf-8?B?" ++ base64:encode_to_string(?MAIL_NEWS_SUBJECT) ++ "?=",
 					 ?MAIL_NEWS_BODY ++
 					 "http://risovaska.ru/unsubscribe?c=" ++ UnsubscribeCode ++ "&u=" ++ integer_to_list(UserId) ++ "\r\n"),
 		moodbox_email:send_mail(Email, Msg, "relay.rinet.ru", "login", "password", false),
 		timer:sleep(100)
 	    catch
 		_:_ ->
 		    io:format("Error sending a letter to ~s~n", [Email])
 	    end
     end.

integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} ->
            old_integer_to_hex(I);
        Int ->
            Int
    end.

old_integer_to_hex(I) when I<10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I<16 ->
    [I-10+$A];
old_integer_to_hex(I) when I>=16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).

url_encode([H|T]) ->
    if
	H >= $a, $z >= H ->
	    [H|url_encode(T)];
	H >= $A, $Z >= H -> % %2F
	    [H|url_encode(T)];
        H >= $0, $9 >= H ->
	    [H|url_encode(T)];
        %H == $_; H == $.; H == $-; H == $: -> % FIXME: more..
        %    [H|url_encode(T)];
        true ->
	    case integer_to_hex(H) of
		[X, Y] ->
		    [$%, X, Y | url_encode(T)];
		[X] ->
		    [$%, $0, X | url_encode(T)]
            end
     end;
 
url_encode([]) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% exceptions handling

get_exception_message(?exception_data(#invalid_user_id{user_id = UserId})) ->
    io_lib:format("Invalid UserId during update user profile: ~p", [UserId]);
get_exception_message(?exception_data(#update_user_picture_exception{user_id = UserId, exception_message = ExceptionMessage})) ->
    io_lib:format("Exception of updating User's avatar with ID: ~p, reason: ~p", [UserId, ExceptionMessage]);
get_exception_message(?exception_data(#get_user_picture_exception{user_id = UserId, exception_message = ExceptionMessage})) ->
    io_lib:format("Exception of getting User's avatar with ID: ~p, reason: ~p", [UserId, ExceptionMessage]);
get_exception_message(?exception_data(#cannot_authorize_yourself_exception{user_id = UserId})) ->
    io_lib:format("Can't authorize yourself, userId: ~p", [UserId]);
get_exception_message(?exception_data(#invalid_notification_message_format{message = Message})) ->
    io_lib:format("Invalid notification message format from notification server: ~p", [Message]);
get_exception_message(?exception_data(#notification_session_not_found{user_id = UserId})) ->
    io_lib:format("There isn't notification session for user_id: ~p", [UserId]);
get_exception_message(?exception_data(#add_image_to_moodstrip_exception{user_id = UserId, moodstrip_id = MoodstripId, exception_message = Exception})) ->
    io_lib:format("Exception of adding an image in moodstrip_id: ~p of user_id: ~p, reason: ~p", [MoodstripId, UserId, Exception]);
get_exception_message(?exception_data(#send_message_exception{user_id = UserId, exception_message = ExceptionMessage})) ->
    io_lib:format("Exception of sending message, author_id: ~p, reason: ~p", [UserId, ExceptionMessage]);
get_exception_message(?exception_data(#publish_image_exception{user_id = UserId, message_id = MessageId, exception_message = ExceptionMessage})) ->
    io_lib:format("Exception of publishing message_id: ~p of user_id: ~p, reason: ~p", [MessageId, UserId, ExceptionMessage]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test functions

-ifndef(NOTEST).

test_delete_account(Login) ->
    F = fun() -> 
	      QH = qlc:q([{U#user_account_data.id} ||
			     U <- mnesia:table(user_account_data),
			     U#user_account_data.login == Login]),
	      QC = qlc:cursor(QH),
	      Result = qlc:next_answers(QC, 1),
	      qlc:delete_cursor(QC),
	      case Result of
		  [{ID}] ->
		      mnesia:delete({user_account_data, ID});
		  _ ->
		      ok
	      end
    end,
    mnesia:transaction(F).

test_notification(UserId) ->
    #notification_registration_result{server = _Server, key = Key} = notification_register(#context{user_id = UserId}),
    moodbox_facade:notification_unregister(#context{user_id = UserId}, Key).

user_id_by_login(Login) ->
    F = fun() -> 
	      QH = qlc:q([U#user_account_data.id ||
			     U <- mnesia:table(user_account_data),
			     U#user_account_data.login == Login]),
	      QC = qlc:cursor(QH),
	      Result = qlc:next_answers(QC, 1),
	      qlc:delete_cursor(QC),
	      case Result of
		  [] ->
		      0;
		  [UserID] ->
		      UserID
	      end
    end,
    {atomic, ID} = mnesia:transaction(F),
    ID.

-endif.

main_test_() ->
    [?_assertMatch( ok, create_account(#context{}, #user_account{id = 0, login = "Test_Actions", password = "magic", 
						name = "Test", email = "vostrjakov@mail.com"}, undefined) ),
     ?_assertMatch( ok, update_account(#context{user_id = 1}, #user_account{id = 1, password = "test", name = "Arty Dude", country = 'RU', 
							     city = "Moscow", email = "arty@artytalk.com",
							     birth_day = moodbox_datetime:from_erlang_datetime({{1976, 09, 03}, {12, 00, 00}})}, 
				       false, undefined, undefined) ),
     ?_assertMatch( ok, update_password(#context{user_id = 1}, "test1", "test1") ),
     ?_assertMatch( Ticket when is_record(Ticket, auth_ticket_result), get_auth_ticket(#context{user_id = 1}, "arty", "test1") ),
     ?_assertMatch( {auth_ticket_result, undefined, undefined, undefined, invalid_credentials, _LockTime},
		    get_auth_ticket(#context{user_id = 1}, "arty", "wrongpassword") ),
     ?_assertMatch( ok, update_user_picture( #context{user_id = user_id_by_login("Test_Actions")}, <<"PictureData">>, "image/png" ) ),
     ?_assertMatch( #user_picture_result{result_code = ok, user_id = 1, last_change_date = _LastChangedDate, picture_data = _PictureData}, 
		    get_user_picture(#context{user_id = 1}, 1, undefined) ),
     ?_assertMatch( contact_not_found, remove_from_contacts(#context{user_id = user_id_by_login("Test_Actions")}, 1) ),
     ?_assertMatch( [], get_contacts(#context{user_id = user_id_by_login("Test_Actions")}) ),
     ?_assertMatch( #user_account{id = 1, login = "arty", password = undefined,
				  creation_date = _CreationDate,
				  motto = [], about_me = [], name = "Arty Dude", country = 'RU',
				  city = "Moscow", email = "arty@artytalk.com", sex = undefined,
				  birth_day = 210600000000000, language = undefined,
				  allow_news = undefined, allow_publishing = undefined,
				  allow_show_friends = true}, 
		    get_my_account(#context{user_id = 1}) ),
     ?_assertMatch( Profile when is_record(Profile, user_info), 
		    get_user_info(#context{user_id = user_id_by_login("Test_Actions")}, user_id_by_login("Test_Actions")) ),
     ?_assertMatch( ok, process_authorization_request(#context{user_id = user_id_by_login("Test_Actions")}, 2, "Hi, Dude!") ),
     ?_assertMatch( ok, process_authorization_response(#context{user_id = 2}, user_id_by_login("Test_Actions"), true) ),
     ?_assertMatch( [Contact] when is_record(Contact, contact_info), 
		    get_contacts(#context{user_id = user_id_by_login("Test_Actions")}) ),
     ?_assertMatch( [Contact] when is_record(Contact, contact_logins_result), 
		    get_contact_logins(#context{user_id = user_id_by_login("Test_Actions")}, ?friend, user_id_by_login("Test_Actions")) ),
     ?_assertMatch( {send_message_result, ok, _MessageId, _CreatedDate},
		    send_friend_message(#context{user_id = user_id_by_login("Test_Actions")}, true, <<"PicureData">>, "") ),
     ?_assertMatch( {send_message_result, ok, _MessageId, _CreatedDate}, 
		    send_friend_message(#context{user_id = user_id_by_login("Test_Actions")}, 2, <<"PicureData">>, "") ),
     ?_assertMatch( ok, block_contact(#context{user_id = user_id_by_login("Test_Actions")}, 2) ),
     ?_assertMatch( ok, unblock_contact(#context{user_id = user_id_by_login("Test_Actions")}, 2) ),
     ?_assertMatch( List when is_list(List), get_published_images(#context{user_id = 1}, ?friend, 1) ),
     ?_assertMatch( Result when is_record(Result, user_search_result), simple_search_contacts(#context{user_id = 1}, 1, 50, "Test_Actions") ),
     ?_assertMatch( Result when is_record(Result, user_search_result), 
		    advanced_search_contacts(#context{user_id = 1}, 1, 50, "arty", 'RU', "Moscow", undefined, 0, 0) ),
     ?_assertMatch( {user_search_result,0,false,[]}, advanced_search_contacts(#context{user_id = 1}, 1, 50, "arty", 'RU', "Moscow", male, 0, 0) ),
     ?_assertMatch( Result when is_record(Result, user_search_result), get_sorted_users(undefined, 1, 1, most_recent, all_time) ),
     ?_assertMatch( ok, reset_password(#context{user_id = 1}, "arty") ),
     ?_assertMatch( [], get_recovered_password(undefined, 1, "123456789q") ),
     ?_assertMatch( unsubscribe_error, unsubscribe(undefined, 1, "test") )
    ].

moodstrip_test_() ->
    {"moodstrip",
     [?_assertMatch( {moodstrip_search_result, 1, false, [{moodstrip_result, 3, 1, "arty","Test MoodStrip 3", 
							 _CreatedDate, "http://s3.amazonaws.com/" ++ ?BUCKET ++ "/moodstrip/3",
							 undefined}]}, 
		     search_moodstrip(undefined, 1, 50, "Test MoodStrip 3") ),
      ?_assertMatch( {moodstrip_search_result,1,false,
		      List} when List =/= [], 
		     get_contact_moodstrips(undefined, 1, 50, ?friend
					    , 1) ),
      ?_assertMatch( {moodstrip_search_result,0,false,[]}, get_contact_moodstrips(undefined, 1, 50, ?friend, 3) ),
      ?_assertMatch( {moodstrip_search_result,1,false, List} when List =/= [], 
		     get_sorted_moodstrips(undefined, 3, 50, most_recent, all_time) ),
      ?_assertMatch( List when List =/= [], get_last_moodstrips(undefined, 50) ),
      ?_assertMatch( {moodstrip_search_result,0,false,[]}, 
		     search_moodstrip(undefined, 1, 50, "AAAAAA") ),
      ?_assertMatch( {moodstrip_result, undefined, 1, "arty", "Test MoodStrip 3",
		       _CreatedDate, undefined,
		       [{moodstrip_item_result,
			 "http://s3.amazonaws.com/" ++ ?BUCKET ++ "/moodstrip/3","test3",
			 _CreatedDate2}]}, 
		     get_moodstrip(undefined, 3, false) )
     ]}.

notification_test_()->
    [
     ?_assertMatch( ok, test_notification(1) )
    ].

server_command_test() ->
    [
     ?_assertMatch( Node when is_list(Node), get_srv_address(#context{user_id = 1}) )
    ].

facade_finish_test_()->
    [
     ?_assertMatch( ok, ?repository:delete_file(?USERPIC_PREFIX, integer_to_list(user_id_by_login("Test_Actions"))) ),
     ?_assertMatch( {atomic, ok}, test_delete_account("Test_Actions") )     
    ].
