%%%-------------------------------------------------------------------
%% @doc UPYUN Erlang SDK - HTTP request module
%%
%% Copyright (c) 2015-present, UPYUN, Inc. All Rights Reserved.
%%
%% This file is released under the MIT license.
%% See the LICENSE for more information.
%%
%%%-------------------------------------------------------------------

-module(upyun_request).

-export([request/4, request/5, request/6]).

-type method() :: get | head | post | put | patch | delete.


-spec request(method(), binary(), binary(), list()) ->
    {ok, any()} | {error, any()}.
request(Method, Endpoint, Uri, Headers) ->
    request(Method, Endpoint, Uri, Headers, []).

-spec request(method(), binary(), binary(), list(), any()) ->
    ok | {ok, any()} | {error, any()}.
request(Method, Endpoint, Uri, Headers, Options)
        when Method =:= get; Method =:= head; Method =:= delete ->
    request(Method, Endpoint, Uri, Headers, <<>>, Options);
request(Method, Endpoint, Uri, Headers, Body) ->
    request(Method, Endpoint, Uri, Headers, Body, []).

-spec request(method(), binary(), binary(), list(), any(), list()) ->
    ok | {ok, any()} | {error, any()}.
request(Method, Endpoint, Uri, Headers0, Body, Options) ->
    Url = << "http://", Endpoint/binary, Uri/binary >>,
    Headers = update_headers(Headers0),
    HackneyOptions = proplists:get_value(hackney, Options, []),
    Response = do_request(Method, Url, Headers, Body, HackneyOptions),
    do_response(Response, proplists:get_value(stream, Options)).

%% @private
do_request(Method, Url, Headers, {stream, Stream}, Options) ->
    case hackney:request(Method, Url, Headers, stream, Options) of
        {ok, Ref} ->
            case hackney:send_body(Ref, Stream) of
                ok ->
                    hackney:start_response(Ref);
                Error ->
                    Error
            end;
        Other ->
            Other
    end;
do_request(Method, Url, Headers, Body, Options) ->
    hackney:request(Method, Url, Headers, Body, Options).

%% @private
%% On Async request.
do_response({ok, _ClientRef}, _) ->
    {error, async_response};
%% On HEAD request if the response succeded.
do_response({ok, StatusCode, Headers0}, _) ->
    Headers = upyun_util:lower_keys(Headers0),
    {ok, {StatusCode, Headers}};
do_response({ok, 200, Headers, ClientRef}, {file, FileName}) ->
    case stream_to_file(FileName, ClientRef) of
        ok -> {ok, {200, Headers}};
        Error -> Error
    end;
do_response({ok, 200, Headers, ClientRef}, Fun) when is_function(Fun) ->
    _ = stream_to_fun(Fun, ClientRef),
    {ok, {200, Headers}};
do_response({ok, StatusCode, Headers0, ClientRef}, _) ->
    case hackney:body(ClientRef) of
        {ok, ResBody} ->
            Headers = upyun_util:lower_keys(Headers0),
            {ok, {StatusCode, Headers, ResBody}};
        Error ->
            Error
    end;
do_response(Error, _) ->
    Error.

%% @private
stream_to_file(FileName, Ref) when is_binary(FileName); is_list(FileName) ->
    {ok, IoDevice} = file:open(FileName, [write, binary]),
    WriteFileFun = fun({ok, Data}) ->
                           file:write(IoDevice, Data);
                      (eof) ->
                           file:close(IoDevice),
                           ok;
                      (Error) ->
                           file:close(IoDevice),
                           Error
                   end,
    stream_to_fun(WriteFileFun, Ref).

%% @private
stream_to_fun(Fun, Ref) when is_function(Fun) ->
    case hackney:stream_body(Ref) of
        {ok, Data} ->
            case Fun({ok, Data}) of
                ok -> stream_to_fun(Fun, Ref);
                Error -> Fun(Error)
            end;
        done ->
            Fun(eof);
        Error ->
            Fun(Error)
    end.

%% @private
update_headers(Headers0) ->
    {ok, Description} = application:get_key(upyun, description),
    {ok, Vsn} = application:get_key(upyun, vsn),
    DescriptionBin = upyun_util:to_binary(Description),
    VsnBin = upyun_util:to_binary(Vsn),
    DefaultUserAgent = <<DescriptionBin/binary, " ", VsnBin/binary>>,
    Headers = upyun_util:lower_keys(Headers0),
    UserAgent = proplists:get_value(<<"user-agent">>, Headers, DefaultUserAgent),
    lists:keymerge(1, Headers, [{<<"user-agent">>, UserAgent}]).
