%%%-------------------------------------------------------------------
%% @doc UPYUN Erlang SDK - public API
%%
%% Copyright (c) 2015-present, UPYUN, Inc. All Rights Reserved.
%%
%% This file is released under the MIT license.
%% See the LICENSE for more information.
%%
%%%-------------------------------------------------------------------

-module(upyun).

-export([init/1]).
%% REST API
-export([put/3, put/4,
         get/2, get/3,
         delete/2, delete/3,
         getinfo/2,
         mkdir/2,
         rmdir/2,
         getlist/2, getlist/3,
         usage/1,
         metadata/3, metadata/4,
         purge/2, purge/3]).

-export_type([client/0,
              type/0,
              put_data/0,
              put_metas/0,
              metadata_option/0,
              list_iter/0,
              list_result/0]).

-type client() :: upyun_config:client().
-type type() :: file | directory.
-type put_data() :: binary() | iolist() | {stream, {file, file:filename_all()} | function()}.
-type put_metas() :: {Width::integer(), Height::integer(), Type::binary(), Frames::integer()}.
-type metadata_option() :: merge | replace | delete.
-type list_iter() :: eof | binary().
-type list_result() :: [[list_result_field()]].
-type list_result_field() :: {name, binary()} |
                             {type, type()} |
                             {size, non_neg_integer()} |
                             {last_modified, non_neg_integer()}.

-spec init(list()) -> {ok, client()}.
init(Options) ->
    application:load(?MODULE),
    {ok, DepApps} = application:get_key(?MODULE, applications),
    _ = [ensure_started(A) || A <- DepApps],
    Client = upyun_config:init(Options),
    {ok, Client}.

%% @private Start the named application if not already started.
-spec ensure_started(App :: atom()) -> ok.
ensure_started(App) ->
    case application:ensure_all_started(App) of
        {ok, _} ->
            ok;
        {error, {_App, _Reason}} ->
            ok
    end.

%% @doc http://docs.upyun.com/api/rest_api/#_2
%% @doc http://docs.upyun.com/api/rest_api/#_3
-spec put(client(), binary(), put_data()) ->
    ok | {ok, put_metas()} | {error, any()}.
put(Client, Key, Data) ->
    put(Client, Key, Data, []).
-spec put(client(), binary(), put_data(), list()) ->
    ok | {ok, put_metas()} | {error, any()}.
put(Client, Key, Data, Options) when is_list(Options) ->
    case proplists:get_value(resume, Options) of
        undefined ->
            upyun_rest:put(Client, Key, Data, Options);
        _ ->
            upyun_resume:put(Client, Key, Data, Options)
    end.

%% @doc http://docs.upyun.com/api/rest_api/#_8
-spec get(client(), binary()) ->
    {ok, any()} | {error, notfound} | {error, any()}.
get(Client, Key) ->
    get(Client, Key, []).
-spec get(client(), binary(), list()) ->
    ok | {ok, any()} | {error, notfound} | {error, any()}.
get(Client, Key, Options) ->
    upyun_rest:get(Client, Key, Options).

%% @doc http://docs.upyun.com/api/rest_api/#_9
-spec delete(client(), binary()) ->
    ok | {error, notfound} | {error, any()}.
delete(Client, Key) ->
    delete(Client, Key, false).
-spec delete(client(), binary(), Async :: boolean()) ->
    ok | {error, notfound} | {error, any()}.
delete(Client, Key, Async) ->
    upyun_rest:delete(Client, Key, Async).

%% @doc http://docs.upyun.com/api/rest_api/#_12
-spec getinfo(client(), binary()) ->
    {ok, {type(), integer(), integer()}} | {error, notfound} | {error, any()}.
getinfo(Client, Key) ->
    upyun_rest:getinfo(Client, Key).

%% @doc http://docs.upyun.com/api/rest_api/#_10
-spec mkdir(client(), binary()) ->
    ok | {error, any()}.
mkdir(Client, Key) ->
    upyun_rest:mkdir(Client, Key).

%% @doc http://docs.upyun.com/api/rest_api/#_11
-spec rmdir(client(), binary()) ->
    ok | {error, notfound} | {error, any()}.
rmdir(Client, Key) ->
    upyun_rest:rmdir(Client, Key).

%% @doc http://docs.upyun.com/api/rest_api/#_13
-spec getlist(client(), binary()) ->
    {ok, {list_result(), list_iter()}} | {error, notfound} | {error, any()}.
getlist(Client, Key) ->
    getlist(Client, Key, []).
-spec getlist(client(), binary(), list()) ->
    {ok, {list_result(), list_iter()}} | {error, notfound} | {error, any()}.
getlist(Client, Key, Options) ->
    upyun_rest:getlist(Client, Key, Options).

%% @doc http://docs.upyun.com/api/rest_api/#_14
-spec usage(client()) ->
    {ok, any()} | {error, any()}.
usage(Client) ->
    upyun_rest:usage(Client).

%% @doc http://docs.upyun.com/api/rest_api/#metadata
-spec metadata(client(), binary(), list()) ->
    ok | {error, notfound} | {error, any()}.
metadata(Client, Key, Metas) ->
    metadata(Client, Key, merge, Metas).
-spec metadata(client(), binary(), metadata_option(), list()) ->
    ok | {error, notfound} | {error, any()}.
metadata(Client, Key, Option, Metas) ->
    metadata(Client, Key, Option, Metas, false).
-spec metadata(client(), binary(), metadata_option(), list(), boolean()) ->
    ok | {error, notfound} | {error, any()}.
metadata(Client, Key, Option, Metas, UpdateLastModified) ->
    upyun_rest:metadata(Client, Key, Option, Metas, UpdateLastModified).

%% @doc http://docs.upyun.com/api/purge/
-spec purge(client(), list()) ->
    {ok, binary()} | {error, any()}.
purge(Client, Keys) ->
    purge(Client, undefined, Keys).
-spec purge(client(), Host :: undefined | binary(), list()) ->
    {ok, binary()} | {error, any()}.
purge(Client, Host, Keys) ->
    upyun_rest:purge(Client, Host, Keys).
