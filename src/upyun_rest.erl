%%%-------------------------------------------------------------------
%% @doc UPYUN Erlang SDK - REST API
%%
%%      More on: http://docs.upyun.com/api/rest_api/
%%
%% Copyright (c) 2015-present, UPYUN, Inc. All Rights Reserved.
%%
%% This file is released under the MIT license.
%% See the LICENSE for more information.
%%
%%%-------------------------------------------------------------------

-module(upyun_rest).

-export([put/4,
         get/3,
         delete/3,
         getinfo/2,
         mkdir/2,
         rmdir/2,
         getlist/3,
         usage/1,
         metadata/5,
         purge/3]).

-type client() :: upyun:client().
-type type() :: upyun:type().
-type put_data() :: upyun:put_data() | {stream, {file, binary(), list()}}.
-type put_metas() :: upyun:put_metas().
-type metadata_option() :: upyun:metadata_option().
-type list_iter() :: upyun:list_iter().
-type list_result() :: upyun:list_result().

-define(CHUNK_SIZE, 4194304). %% 4 MB is the default


-spec put(client(), binary(), put_data(), list()) ->
    ok | {ok, Metas :: put_metas()} | {error, any()}.
put(Client, Key, {stream, {file, FileName}}, Options) ->
    ChunkSize = proplists:get_value(chunk_size, Options, ?CHUNK_SIZE),
    Data = {stream, {file, FileName, [{chunk_size, ChunkSize}]}},
    put(Client, Key, Data, Options);
put(Client, Key, Data, Options) ->
    #{bucket := Bucket, operator := Operator, password := Password,
      endpoint := Endpoint} = Client,
    MD5 = proplists:get_bool(checksum, Options)
          andalso is_binary(Data)
          andalso upyun_auth:md5(Data),
    Secret = proplists:get_value(secret, Options, false),
    Headers0 = upyun_util:lower_keys(
                 proplists:get_value(headers, Options, [])),
    Uri = to_uri(Bucket, Key),
    Headers = authorization_date(Operator, Password, <<"PUT">>, Uri, MD5)
              ++ [{<<"content-md5">>, MD5} || MD5 =/= false]
              ++ [{<<"content-secret">>, Secret} || Secret =/= false],
    ReqHeaders = lists:keymerge(1, Headers0, Headers),
    case upyun_request:request(put, Endpoint, Uri, ReqHeaders, Data) of
        {ok, {200, ResHeaders, _}} ->
            put_response(Headers0, ResHeaders);
        {ok, Other} ->
            {error, Other};
        Error ->
            Error
    end.

%% @private
%%  Maybe preprocessing when uploading image:
%%  http://docs.upyun.com/api/rest_api/#_2
put_response([], _) -> ok;
put_response([{<<"x-gmkerl-", _/binary>>, _} | _], ResHeaders) ->
    Width = upyun_util:to_integer(
              proplists:get_value(<<"x-upyun-width">>, ResHeaders)),
    Height = upyun_util:to_integer(
               proplists:get_value(<<"x-upyun-height">>, ResHeaders)),
    Type = proplists:get_value(<<"x-upyun-file-type">>, ResHeaders),
    Frames = upyun_util:to_integer(
               proplists:get_value(<<"x-upyun-frames">>, ResHeaders)),
    {ok, {Width, Height, Type, Frames}};
put_response([_ | Rest], ResHeaders) ->
    put_response(Rest, ResHeaders).

-spec get(client(), binary(), list()) ->
    ok | {ok, any()} | {error, notfound} | {error, any()}.
get(Client, Key, Options) ->
    #{bucket := Bucket, operator := Operator, password := Password,
      endpoint := Endpoint} = Client,
    Uri = to_uri(Bucket, Key),
    Headers = authorization_date(Operator, Password, <<"GET">>, Uri),
    case upyun_request:request(get, Endpoint, Uri, Headers, Options) of
        {ok, {200, ResHeaders}} ->
            get_response(Options, ResHeaders, false);
        {ok, {200, ResHeaders, Data}} ->
            get_response(Options, ResHeaders, Data);
        {ok, {404, _, _}} ->
            {error, notfound};
        {ok, Other} ->
            {error, Other};
        Error ->
            Error
    end.

%% @private
get_response(Options, ResHeaders, Data) ->
    Metadatas = proplists:get_bool(with_metadata, Options) andalso get_metadatas(ResHeaders),
    get_response(Data, Metadatas).
get_response(false, false) ->
    ok;
get_response(false, Metadatas) ->
    {ok, Metadatas};
get_response(Data, false) ->
    {ok, Data};
get_response(Data, Metadatas) ->
    {ok, {Data, Metadatas}}.

%% @private
get_metadatas(ResHeaders) ->
    lists:foldl(fun({<<"x-upyun-meta-", K/binary>>, V}, Acc) ->
                        [{K, V} | Acc];
                   (_, Acc) ->
                        Acc
                end, [], ResHeaders).

-spec delete(client(), binary(), Async :: boolean()) ->
    ok | {error, notfound} | {error, any()}.
delete(Client, Key, Async) ->
    #{bucket := Bucket, operator := Operator, password := Password,
      endpoint := Endpoint} = Client,
    Uri = to_uri(Bucket, Key),
    Headers = authorization_date(Operator, Password, <<"DELETE">>, Uri)
              ++ [{<<"x-upyun-async">>, true} || Async],
    case upyun_request:request(delete, Endpoint, Uri, Headers) of
        {ok, {200, _, _}} ->
            ok;
        {ok, {404, _, _}} ->
            {error, notfound};
        {ok, Other} ->
            {error, Other};
        Error ->
            Error
    end.

-spec getinfo(client(), binary()) ->
    {ok, {type(), integer(), integer()}} | {error, notfound} | {error, any()}.
getinfo(Client, Key) ->
    #{bucket := Bucket, operator := Operator, password := Password,
      endpoint := Endpoint} = Client,
    Uri = to_uri(Bucket, Key),
    Headers = authorization_date(Operator, Password, <<"HEAD">>, Uri),
    case upyun_request:request(head, Endpoint, Uri, Headers) of
        {ok, {200, ResHeaders}} ->
            Type =
                case proplists:get_value(<<"x-upyun-file-type">>, ResHeaders) of
                    <<"file">> -> file;
                    <<"folder">> -> directory
                end,
            Size = upyun_util:to_integer(
                     proplists:get_value(<<"x-upyun-file-size">>, ResHeaders, 0)),
            LastModified = upyun_util:to_integer(
                             proplists:get_value(<<"x-upyun-file-date">>, ResHeaders)),
            {ok, {Type, Size, LastModified}};
        {ok, {404, _}} ->
            {error, notfound};
        {ok, Other} ->
            {error, Other};
        Error ->
            Error
    end.

-spec mkdir(client(), binary()) ->
    ok | {error, any()}.
mkdir(Client, Key) ->
    #{bucket := Bucket, operator := Operator, password := Password,
      endpoint := Endpoint} = Client,
    Uri = to_uri(Bucket, Key),
    Headers = authorization_date(Operator, Password, <<"POST">>, Uri)
              ++ [{<<"folder">>, <<"true">>}],
    case upyun_request:request(post, Endpoint, Uri, Headers) of
        {ok, {200, _, _}} ->
            ok;
        {ok, Other} ->
            {error, Other};
        Error ->
            Error
    end.

-spec rmdir(client(), binary()) ->
    ok | {error, notfound} | {error, any()}.
rmdir(Client, Key) ->
    #{bucket := Bucket, operator := Operator, password := Password,
      endpoint := Endpoint} = Client,
    Uri = to_uri(Bucket, Key),
    Headers = authorization_date(Operator, Password, <<"DELETE">>, Uri),
    case upyun_request:request(delete, Endpoint, Uri, Headers) of
        {ok, {200, _, _}} ->
            ok;
        {ok, {404, _, _}} ->
            {error, notfound};
        {ok, Other} ->
            {error, Other};
        Error ->
            Error
    end.

-spec getlist(client(), binary(), list()) ->
    {ok, {list_result(), list_iter()}} | {error, notfound} | {error, any()}.
getlist(Client, Key, Options) ->
    #{bucket := Bucket, operator := Operator, password := Password,
      endpoint := Endpoint, timeout := Timeout0} = Client,
    Uri = to_uri(Bucket, Key),
    Headers = authorization_date(Operator, Password, <<"GET">>, Uri),
    Headers2 =
        case proplists:get_value(iter, Options) of
            undefined ->
                Headers;
            Iter ->
                Headers ++ [{<<"x-list-iter">>, Iter}]
        end,
    Headers3 =
        case proplists:get_value(limit, Options) of
            undefined ->
                Headers2;
            Limit when is_integer(Limit) ->
                Headers2 ++ [{<<"x-list-limit">>, Limit}]
        end,
    Headers4 =
        case proplists:get_value(order, Options) of
            undefined ->
                Headers3;
            Order when Order =:= asc orelse Order =:= desc ->
                Order2 = erlang:atom_to_binary(Order, utf8),
                Headers3 ++ [{<<"x-list-order">>, Order2}]
        end,
    ReqTimeout =
        case proplists:get_value(timeout, Options) of
            undefined ->
                Timeout0;
            Timeout ->
                Timeout
        end,
    ReqOptions = [{hackney, [{recv_timeout, ReqTimeout}]}],
    case upyun_request:request(get, Endpoint, Uri, Headers4, ReqOptions) of
        {ok, {200, _, <<>>}} ->
            {ok, {[], eof}};
        {ok, {200, ResHeaders, ResBody}} ->
            ResIter =
                case proplists:get_value(<<"x-upyun-list-iter">>, ResHeaders) of
                    <<"g2gCZAAEbmV4dGQAA2VvZg">> -> eof;
                    Other -> Other
                end,
            Lines = binary:split(ResBody, <<"\n">>, [global]),
            Res = [lists:zip([name, type, size, last_modified],
                             to_values(binary:split(Line, <<"\t">>, [global])))
                   || Line <- Lines],
            {ok, {Res, ResIter}};
        {ok, {404, _, _}} ->
            {error, notfound};
        {ok, Other} ->
            {error, Other};
        Error ->
            Error
    end.

%% @private
to_values([Name, <<"N">>, Size, LastModified]) ->
    to_values([Name, file, Size, LastModified]);
to_values([Name, <<"F">>, Size, LastModified]) ->
    to_values([Name, directory, Size, LastModified]);
to_values([Name, Type, Size, LastModified]) when is_binary(Size) ->
    to_values([Name, Type, upyun_util:to_integer(Size), LastModified]);
to_values([Name, Type, Size, LastModified]) when is_binary(LastModified) ->
    to_values([Name, Type, Size, upyun_util:to_integer(LastModified)]);
to_values(Values) -> Values.

-spec usage(client()) ->
    {ok, any()} | {error, any()}.
usage(Client) ->
    #{bucket := Bucket, operator := Operator, password := Password,
      endpoint := Endpoint} = Client,
    Uri = <<$/, Bucket/binary, $/, "?usage">>,
    Headers = authorization_date(Operator, Password, <<"GET">>, Uri),
    case upyun_request:request(get, Endpoint, Uri, Headers) of
        {ok, {200, _, ResBody}} ->
            {ok, ResBody};
        {ok, Other} ->
            {error, Other};
        Error ->
            Error
    end.

-spec metadata(client(), binary(), Option :: metadata_option(), Metas :: list(), boolean()) ->
    ok | {error, notfound} | {error, any()}.
metadata(Client, Key, Option, Metas, UpdateLastModified) ->
    #{bucket := Bucket, operator := Operator, password := Password,
      endpoint := Endpoint} = Client,
    Param = case UpdateLastModified of
                true -> <<"&update_last_modified=true">>;
                false -> <<>>
            end,
    Uri = to_uri(Bucket, Key, << $?, "metadata=",(upyun_util:to_binary(Option))/binary, Param/binary >>),
    %% Normalize meta K-Vs
    UpdatedMetas = case Option of
                       replace -> [{upyun_util:to_binary(K), true} || {K, _} <- Metas];
                       _       -> [{upyun_util:to_binary(K), V} || {K, V} <- Metas]
                   end,
    Headers = authorization_date(Operator, Password, <<"PATCH">>, Uri)
              ++ [{<<"x-upyun-meta-", K/binary>>, V} || {K, V} <- UpdatedMetas],
    case upyun_request:request(patch, Endpoint, Uri, Headers) of
        {ok, {200, _, _}} ->
            ok;
        {ok, {404, _, _}} ->
            {error, notfound};
        {ok, Other} ->
            {error, Other};
        Error ->
            Error
    end.

%% @private
to_uri(Bucket, Key) ->
    to_uri(Bucket, Key, <<>>).
to_uri(Bucket, Key, Suffix) ->
    << $/, Bucket/binary, (upyun_util:urlencode(Key))/binary, Suffix/binary >>.

%% @private
authorization_date(Operator, Password, Method, Uri) ->
    authorization_date(Operator, Password, Method, Uri, false).
authorization_date(Operator, Password, Method, Uri, MD5) ->
    Date = upyun_util:rfc1123(),
    Sign = upyun_auth:sign_rest(Operator, Password, Method, Uri, Date, MD5),
    [{<<"authorization">>, Sign}, {<<"date">>, Date}].


-spec purge(client(), Host0 :: undefined | binary(), Keys0 :: list()) ->
    {ok, binary()} | {error, any()}.
purge(Client, Host0, Keys0) ->
    #{bucket := Bucket, operator := Operator,
      password := Password} = Client,
    Host = case Host0 of
               undefined ->
                   <<Bucket/binary, ".b0.upaiyun.com">>;
               _ ->
                   Host0
           end,
    Urls = [<<"http://", Host/binary, $/, K/binary>> || K <- Keys0],
    UrlsBin = upyun_util:join(Urls, <<"\n">>),
    Endpoint = <<"purge.upyun.com">>,
    Uri = <<"/purge/">>,
    Date = upyun_util:rfc1123(),
    Sign = upyun_auth:sign_purge(Operator, Password, Bucket, UrlsBin, Date),
    Headers = [{<<"authorization">>, Sign},
               {<<"date">>, Date},
               {<<"content-type">>, <<"application/x-www-form-urlencoded">>},
               {<<"accept">>, <<"application/json">>}],
    Body = <<"purge=", UrlsBin/binary>>,
    case upyun_request:request(post, Endpoint, Uri, Headers, Body) of
        {ok, {200, _, JsonBin}} ->
            {ok, JsonBin};
        {ok, Other} ->
            {error, Other};
        Error ->
            Error
    end.
