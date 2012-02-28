-module(moodbox_repository).

-export([init/0, put_object/3, put_object/6, get_file_with_header/2, get_file_with_header/3, get_file/2, get_file/3, get_attributes/2, 
	 get_url/2, delete_file/2]).

-include_lib("eunit/include/eunit.hrl").

-include("moodbox_repository.hrl").

put_object(Prefix, Id, Data) ->
    put_object(Prefix, Id, Data, "image/png", [], false).

-ifdef(LOCAL_REPOSITORY).

init() ->
    mnesia:start().

put_object(Prefix, Id, Data, _ContentType, _Attributes, _IsPublic) ->
    file:write_file(?LOCAL_DIRECTORY ++ Prefix ++ integer_to_list(Id), Data),
    ok.

get_file_with_header(Prefix, FileName) ->
    {ok, Data} = file:read_file(?LOCAL_DIRECTORY ++ Prefix ++ FileName),
    {ok, [], Data}.

get_file(Prefix, FileName) ->
    file:read_file(?LOCAL_DIRECTORY ++ Prefix ++ FileName).

delete_file(Prefix, FileName) ->
    file:delete(?LOCAL_DIRECTORY ++ Prefix ++ FileName).

get_attributes(Prefix, FileName) ->
    {ok, []}. % attributes is saved only in S3

get_url(_Prefix, _Id) ->
    "".

-else.

init() ->
    erlaws:start(),
    mnesia:start(),
    S3 = erlaws_s3:new(?AWS_KEY, ?AWS_SEC_KEY, ?AWS_SECURE),
    {ok, List, _Request} = S3:list_buckets(),
    case lists:member(?BUCKET, List) of
	true ->
	    ok;
	false ->
	    S3:create_bucket(?BUCKET)
    end,
    ssl:seed(crypto:rand_bytes(256)).

put_object(Prefix, Id, Data, ContentType, Attributes, IsPublic) ->
    S3 = erlaws_s3:new(?AWS_KEY, ?AWS_SEC_KEY, ?AWS_SECURE),
    case S3:put_object(?BUCKET, Prefix ++ integer_to_list(Id), Data, ContentType, Attributes, IsPublic) of
       {ok, _Info, _Request} ->
	    ok;
       Error ->
	    Error
    end.

get_file_with_header(Prefix, FileName) ->
    get_file_with_header(Prefix, FileName, undefined).

get_file_with_header(Prefix, FileName, LastModifiedDate) ->
    S3 = erlaws_s3:new(?AWS_KEY, ?AWS_SEC_KEY, ?AWS_SECURE),
    S3:get_object_with_header(?BUCKET, Prefix ++ FileName, LastModifiedDate).

get_file(Prefix, FileName) ->
    get_file(Prefix, FileName, undefined).

get_file(Prefix, FileName, LastModifiedDate) ->
    S3 = erlaws_s3:new(?AWS_KEY, ?AWS_SEC_KEY, ?AWS_SECURE),
    case S3:get_object(?BUCKET, Prefix ++ FileName, LastModifiedDate) of
	{ok, Data, _Request} ->
	    {ok, Data};
	Error ->
	    Error
    end.

delete_file(Prefix, FileName) ->
    S3 = erlaws_s3:new(?AWS_KEY, ?AWS_SEC_KEY, ?AWS_SECURE),
    case S3:delete_object(?BUCKET, Prefix ++ FileName) of
	{ok, _Request} ->
	    ok;
	Error ->
	    Error
    end.

get_attributes(Prefix, FileName) ->
    S3 = erlaws_s3:new(?AWS_KEY, ?AWS_SEC_KEY, ?AWS_SECURE),
    case S3:info_object(?BUCKET, Prefix ++ FileName) of
	{ok, Attributes, _RequestId} ->
	    {ok, Attributes};
	Error ->
	    Error
    end.

get_url(Prefix, Id) ->
    "http://" ++ ?AWS_S3_HOST ++ "/" ++ ?BUCKET ++ "/" ++ Prefix ++ integer_to_list(Id).

-endif.
