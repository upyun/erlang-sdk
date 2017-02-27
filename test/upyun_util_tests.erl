-module(upyun_util_tests).
-include_lib("eunit/include/eunit.hrl").

join_test() ->
    Items = [1, 2, 3, <<"a">>, <<"b">>, <<"c">>],
    Expected = <<"1, 2, 3, a, b, c">>,
    ?assertEqual(Expected, upyun_util:join(Items, <<", ">>)),

    Items2 = [<<"bucket">>, <<"path/to">>, <<"filename">>],
    Expected2 = <<"bucket/path/to/filename">>,
    ?assertEqual(Expected2, upyun_util:join(Items2, <<"/">>)),

    Items3 = [<<"bucket">>],
    Expected3 = <<"bucket">>,
    ?assertEqual(Expected3, upyun_util:join(Items3, <<"/">>)),

    Items4 = [],
    Expected4 = <<>>,
    ?assertEqual(Expected4, upyun_util:join(Items4, <<"/">>)).

to_binary_test() ->
    %% {Term, Result}.
    Tests = [{undefined, <<>>},
             {<<"a">>, <<"a">>},
             {"a", <<"a">>},
             {a, <<"a">>},
             {1, <<"1">>},
             {1.0, <<"1.0">>}],
    [R = upyun_util:to_binary(X) || {X, R} <- Tests].

to_integer_test() ->
    1 = upyun_util:to_integer(1),
    1 = upyun_util:to_integer("1"),
    1 = upyun_util:to_integer(<<"1">>).

lower_keys_test() ->
    Headers = [{<<"Authorization">>, <<>>},
               {<<"Content-Length">>, 0},
               {<<"User-Agent">>, <<"Erlang-SDK">>},
               {<<"X-UPYUN-ABC">>, <<"abc">>}],
    ExpectedHeaders = [{<<"authorization">>, <<>>},
                       {<<"content-length">>, 0},
                       {<<"user-agent">>, <<"Erlang-SDK">>},
                       {<<"x-upyun-abc">>, <<"abc">>}],
    ?assertEqual(ExpectedHeaders, upyun_util:lower_keys(Headers)).

urlencode_test() ->
    %% {Url, Result}.
    Tests = [{<<"0123456789">>, <<"0123456789">>},
             {<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>, <<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>},
             {<<"abcdefghijklmnopqrstuvwxyz">>, <<"abcdefghijklmnopqrstuvwxyz">>},
             {<<"~_-.:@+,;=">>, <<"~_-.:@+,;=">>},
             {<<"?#<>()[]{}!$&'*%`^">>, <<"%3f%23%3c%3e%28%29%5b%5d%7b%7d%21%24%26%27%2a%25%60%5e">>},
             {<<" ", $\s>>, <<"++">>},
             {<<"你好，又拍云"/utf8>>, <<"%e4%bd%a0%e5%a5%bd%ef%bc%8c%e5%8f%88%e6%8b%8d%e4%ba%91">>}],
    [R = upyun_util:urlencode(U) || {U, R} <- Tests].

rfc1123_test() ->
    Date = {{2017, 02, 14}, {12, 34, 56}},
    ExpectedDate = <<"Tue, 14 Feb 2017 12:34:56 GMT">>,
    ?assertEqual(ExpectedDate, upyun_util:rfc1123(Date)).
