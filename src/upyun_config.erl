%%%-------------------------------------------------------------------
%% @doc UPYUN Erlang SDK - Configuration provider
%%
%% Copyright (c) 2015-present, UPYUN, Inc. All Rights Reserved.
%%
%% This file is released under the MIT license.
%% See the LICENSE for more information.
%%
%%%-------------------------------------------------------------------

-module(upyun_config).

%% API
-export([init/1]).

-type client() :: #{bucket => binary(),
                    endpoint => binary(),
                    operator => binary(),
                    password => binary(),
                    timeout => integer()}.

-define(DEFAULT_ENDPOINT, auto).
-define(DEFAULT_TIMEOUT, 60000). %% 1 minute


-spec init(list()) ->
    client() | no_return().
init(Options) when is_list(Options) ->
    ensure_exist(Options, bucket),
    ensure_exist(Options, operator),
    ensure_exist(Options, password),
    Defaults = #{endpoint => ?DEFAULT_ENDPOINT,
                 timeout => ?DEFAULT_TIMEOUT},
    update_endpoint(
      maps:merge(Defaults,
                 maps:from_list(Options))).

%% @private
ensure_exist(Options, Name) ->
    Exist = proplists:is_defined(Name, Options),
    Error = << "Missing config option: ", (upyun_util:to_binary(Name))/binary >>,
    not Exist andalso erlang:error(Error).

%% @private
update_endpoint(Config) ->
    case maps:find(endpoint, Config) of
        error ->
            Config;
        {ok, Endpoint} ->
            maps:update(endpoint, endpoint(Endpoint), Config)
    end.

%% @private
endpoint(auto)    -> <<"v0.api.upyun.com">>;
endpoint(telecom) -> <<"v1.api.upyun.com">>;
endpoint(cnc)     -> <<"v2.api.upyun.com">>;
endpoint(ctt)     -> <<"v3.api.upyun.com">>.
