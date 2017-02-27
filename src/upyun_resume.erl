%%%-------------------------------------------------------------------
%% @doc UPYUN Erlang SDK - Upload and resume file fragments
%%
%% Copyright (c) 2015-present, UPYUN, Inc. All Rights Reserved.
%%
%% This file is released under the MIT license.
%% See the LICENSE for more information.
%%
%%%-------------------------------------------------------------------

-module(upyun_resume).

-export([put/4]).
-export([initiate/5,
         upload/6,
         complete/4]).

-type client() :: upyun:client().

-define(MAX_PART_SIZE, 1048576). % 1M
%% Maximum number of executing uploading request.
-define(MAX_RETRIES, 2).
-define(RETRY_DELAY, 100). % in milliseconds


-spec put(client(), binary(), binary(), list()) ->
    ok | {ok, any()} | {error, any()}.
put(Client, Key, FileBin, Options) when is_binary(FileBin) ->
    Type = proplists:get_value(type, Options),
    MD5 = proplists:get_bool(checksum, Options),
    Length = byte_size(FileBin),
    case initiate(Client, Key, Type, Length, Options) of
        {ok, {UploadId, PartId}} ->
            case do_put(FileBin, {Client, Key, MD5, UploadId, PartId}) of
                {ok, {UploadId, _}} ->
                    case complete(Client, Key, UploadId, maybe_md5(MD5, FileBin)) of
                        {ok, {UploadId, Type, Length}} ->
                            ok;
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec initiate(client(), binary(), binary(), integer(), list()) ->
    {ok, any()} | {error, any()}.
initiate(Client, Key, Type, Length, Options) ->
    #{bucket := Bucket, operator := Operator, password := Password,
      endpoint := Endpoint} = Client,
    Uri = <<$/, Bucket/binary, Key/binary>>,
    Headers0 = proplists:get_value(headers, Options, []),
    Date = upyun_util:rfc1123(),
    Sign = upyun_auth:sign_rest(Operator, Password, <<"PUT">>, Uri, Date),
    Headers = [{<<"authorization">>, Sign},
               {<<"date">>, Date},
               {<<"content-length">>, 0},
               {<<"x-upyun-multi-stage">>, initiate},
               {<<"x-upyun-multi-type">>, Type},
               {<<"x-upyun-multi-length">>, Length}],
    ReqHeaders = lists:keymerge(1, Headers0, Headers),
    case upyun_request:request(put, Endpoint, Uri, ReqHeaders) of
        {ok, {204, ResHeaders, _}} ->
            UploadId = proplists:get_value(<<"x-upyun-multi-uuid">>, ResHeaders),
            PartId = proplists:get_value(<<"x-upyun-next-part-id">>, ResHeaders),
            {ok, {UploadId, PartId}};
        {ok, Other} ->
            {error, Other};
        Error ->
            Error
    end.

-spec upload(client(), binary(), binary(), binary(), binary(), boolean()) ->
    {ok, any()} | {error, any()}.
upload(Client, Key, UploadId, PartId, FilePart, MD5) ->
    #{bucket := Bucket, operator := Operator, password := Password,
      endpoint := Endpoint} = Client,
    Uri = <<$/, Bucket/binary, Key/binary>>,
    Date = upyun_util:rfc1123(),
    Sign = upyun_auth:sign_rest(Operator, Password, <<"PUT">>, Uri, Date),
    Headers = [{<<"authorization">>, Sign},
               {<<"date">>, Date},
               {<<"content-length">>, byte_size(FilePart)},
               {<<"x-upyun-multi-stage">>, upload},
               {<<"x-upyun-multi-uuid">>, UploadId},
               {<<"x-upyun-part-id">>, PartId}]
              ++ [{<<"content-md5">>, MD5} || MD5 =/= false],
    case upyun_request:request(put, Endpoint, Uri, Headers, FilePart) of
        {ok, {204, ResHeaders, _}} ->
            ResUploadId = proplists:get_value(<<"x-upyun-multi-uuid">>, ResHeaders),
            ResPartId = proplists:get_value(<<"x-upyun-next-part-id">>, ResHeaders),
            {ok, {ResUploadId, ResPartId}};
        {ok, Other} ->
            {error, Other};
        Error ->
            Error
    end.

-spec complete(client(), binary(), binary(), boolean()) ->
    {ok, any()} | {error, any()}.
complete(Client, Key, UploadId, MD5) ->
    #{bucket := Bucket, operator := Operator, password := Password,
      endpoint := Endpoint} = Client,
    Uri = <<$/, Bucket/binary, Key/binary>>,
    Date = upyun_util:rfc1123(),
    Sign = upyun_auth:sign_rest(Operator, Password, <<"PUT">>, Uri, Date),
    Headers = [{<<"authorization">>, Sign},
               {<<"date">>, Date},
               {<<"content-length">>, 0},
               {<<"x-upyun-multi-stage">>, complete},
               {<<"x-upyun-multi-uuid">>, UploadId}]
              ++ [{<<"x-upyun-multi-md5">>, MD5} || MD5 =/= false],
    case upyun_request:request(put, Endpoint, Uri, Headers) of
        {ok, {Code, ResHeaders, _}} when Code =:= 201 orelse Code =:= 204 ->
            ResUploadId = proplists:get_value(<<"x-upyun-multi-uuid">>, ResHeaders),
            ResType = proplists:get_value(<<"x-upyun-multi-type">>, ResHeaders),
            ResLength = upyun_util:to_integer(
                          proplists:get_value(<<"x-upyun-multi-length">>, ResHeaders)),
            {ok, {ResUploadId, ResType, ResLength}};
        {ok, Other} ->
            {error, Other};
        Error ->
            Error
    end.

%% private
maybe_md5(false, _) -> false;
maybe_md5(_, Bin) -> upyun_auth:md5(Bin).

%% @private
do_put(Bin, {Client, Key, MD5, UploadId, PartId})
        when byte_size(Bin) > ?MAX_PART_SIZE ->
    << Part:?MAX_PART_SIZE/binary, Rest/binary>> = Bin,
    Args = [Client, Key, UploadId, PartId, Part, maybe_md5(MD5, Part)],
    case retry_upload(Args) of
        {ok, {UploadId, ResPartId}} ->
            do_put(Rest, {Client, Key, MD5, UploadId, ResPartId});
        Error ->
            Error
    end;
do_put(Bin, {Client, Key, MD5, UploadId, PartId}) ->
    retry_upload([Client, Key, UploadId, PartId, Bin, maybe_md5(MD5, Bin)]).

%% @private
retry_upload(Args) ->
    retry_upload(?MAX_RETRIES, Args).
retry_upload(Retry, Args) ->
    case erlang:apply(?MODULE, upload, Args) of
        {ok, _}=Result ->
            Result;
        _Other when Retry > 1 ->
            timer:sleep(?RETRY_DELAY),
            retry_upload(Retry - 1, Args);
        Error ->
            Error
    end.
