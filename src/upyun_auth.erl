%%%-------------------------------------------------------------------
%% @doc UPYUN Erlang SDK - Authorization module
%%
%% Copyright (c) 2015-present, UPYUN, Inc. All Rights Reserved.
%%
%% This file is released under the MIT license.
%% See the LICENSE for more information.
%%
%%%-------------------------------------------------------------------

-module(upyun_auth).

-export([sign_rest/5, sign_rest/6]).
-export([sign_purge/5]).
-export([md5/1]).


%% @doc REST API Signature
%%      http://docs.upyun.com/api/authorization/#_2
-spec sign_rest(binary(), binary(), binary(), binary(), Date :: binary()) ->
    binary().
sign_rest(Operator, Password, Method, Uri, Date) ->
    sign_rest(Operator, Password, Method, Uri, Date, false).

-spec sign_rest(binary(), binary(), binary(), binary(), Date :: binary(),
                MD5 :: false | binary()) -> binary().
sign_rest(Operator, Password, Method, Uri, Date, MD5) ->
    EncodedPassword = hex_str(md5(Password)),
    RawBin = upyun_util:join(([Method, Uri, Date] ++ [MD5 || MD5 =/= false]), <<"&">>),
    Signature = base64:encode(sha1_hmac(EncodedPassword, RawBin)),
    << "UPYUN ", Operator/binary, $:, Signature/binary >>.

%% @doc Purge API Signature
%%      http://docs.upyun.com/api/purge/#_1
-spec sign_purge(binary(), binary(), binary(), binary(), binary()) ->
    binary().
sign_purge(Operator, Password, Bucket, UrlsBin, Date) ->
    EncodedPassword = hex_str(md5(Password)),
    RawBin = upyun_util:join([UrlsBin, Bucket, Date, EncodedPassword], <<"&">>),
    Signature = hex_str(md5(RawBin)),
    << "UpYun ", Bucket/binary, $:, Operator/binary, $:, Signature/binary >>.

-spec md5(binary()) -> binary().
md5(Data) ->
    crypto:hash(md5, Data).

%% @private
hex_str(B) when is_binary(B) ->
    T = {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b, $c, $d, $e, $f},
    << <<(element(X bsr 4 + 1, T)), (element(X band 16#0F + 1, T))>>
       || <<X:8>> <= B >>.

%% @private
sha1_hmac(Key, Data) ->
    sha_mac(Key, Data).

%% @private
-ifndef(old_hash).
sha_mac(Key, Data) ->
    crypto:hmac(sha, Key, Data).
-else.
sha_mac(Key, Data) ->
    crypto:sha_mac(Key, Data).
-endif.
