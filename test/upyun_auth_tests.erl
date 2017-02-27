-module(upyun_auth_tests).
-include_lib("eunit/include/eunit.hrl").

sign_rest_test() ->
    Sign = upyun_auth:sign_rest(<<"operator">>, <<"password">>, <<"PUT">>,
                                <<"/tests/image.jpg">>,
                                <<"Tue, 14 Feb 2017 12:34:56 GMT">>,
                                <<"7ac66c0f148de9519b8bd264312c4d64">>),
    ExpectedSign = <<"UPYUN operator:s/zhp1UjLkRqh3pLgwyeu3orVs0=">>,
    ?assertEqual(ExpectedSign, Sign).

sign_purge_test() ->
    Sign = upyun_auth:sign_purge(<<"operator">>, <<"password">>, <<"bucket">>,
                                 <<"http://bucket.b0.upaiyun.com/tests/image.jpg">>,
                                 <<"Tue, 14 Feb 2017 12:34:56 GMT">>),
    ExpectedSign = <<"UpYun bucket:operator:270efa454970e4d7be5307041d1f30cb">>,
    ?assertEqual(ExpectedSign, Sign).
