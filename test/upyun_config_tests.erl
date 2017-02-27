-module(upyun_config_tests).
-include_lib("eunit/include/eunit.hrl").

config_test() ->
    Options = [{bucket, <<"bucket">>},
               {operator, <<"operator">>},
               {password, <<"password">>}],
    Client = upyun_config:init(Options),
    ExpectedClient = #{bucket => <<"bucket">>,
                       endpoint => <<"v0.api.upyun.com">>,
                       operator => <<"operator">>,
                       password => <<"password">>,
                       timeout => 60000},
    ?assertEqual(Client, ExpectedClient),

    Options2 = [{endpoint, cnc}, {timeout, 10000} | Options],
    Client2 = upyun_config:init(Options2),
    ExpectedClient2 = #{bucket => <<"bucket">>,
                        endpoint => <<"v2.api.upyun.com">>,
                        operator => <<"operator">>,
                        password => <<"password">>,
                        timeout => 10000},
    ?assertEqual(Client2, ExpectedClient2).

missing_config_test() ->
    Options = [{operator, <<"operator">>},
               {password, <<"password">>}],
    Error = <<"Missing config option: bucket">>,
    ?assertError(Error, upyun_config:init(Options)),

    Options2 = [{bucket, <<"bucket">>},
                {password, <<"password">>}],
    Error2 = <<"Missing config option: operator">>,
    ?assertError(Error2, upyun_config:init(Options2)),

    Options3 = [{bucket, <<"bucket">>},
                {operator, <<"operator">>}],
    Error3 = <<"Missing config option: password">>,
    ?assertError(Error3, upyun_config:init(Options3)).

wrong_endpoint_test() ->
    Options = [{bucket, <<"bucket">>},
               {operator, <<"operator">>},
               {password, <<"password">>},
               {endpoint, wrong_endpoint}],
    ?assertError(function_clause, upyun_config:init(Options)).
