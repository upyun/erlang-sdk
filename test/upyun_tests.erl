-module(upyun_tests).
-include_lib("eunit/include/eunit.hrl").

-define(BUCKET, <<"erlang-sdk">>).
-define(OPERATOR, <<"operator">>).
-define(PASSWORD, <<"password">>).
-define(FILE_NAME, "./test/image.jpg").
-define(FILE_TYPE, <<"image/jpg">>).

all_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(Client) ->
         {inorder, all_tests(Client)}
     end}.

setup() ->
    {A1, A2, A3} = erlang:timestamp(),
    random:seed(A1, A2, A3),

    Options = [{bucket, ?BUCKET},
               {operator, ?OPERATOR},
               {password, ?PASSWORD},
               {endpoint, auto}],
    {ok, Client} = upyun:init(Options),
    Client.

teardown(Client) ->
    upyun:rmdir(Client, <<"/tests">>),
    ok.

%% Helper functions

-define(CHARS, "12345678910ABCDEFGHIJKLMNOPQRSTUVWXYZ").
gen_key() -> gen_key(10).
gen_key(Len) ->
    R = erlang:iolist_to_binary(
          lists:map(fun(_) ->
                            lists:nth(random:uniform(length(?CHARS)), ?CHARS)
                    end, lists:seq(1, Len))),
    <<"/tests/", R/binary>>.

get_metadata(Client, Key) ->
    {ok, {_, Metadatas}} = upyun:get(Client, Key, [{with_metadata, true}]),
    Metadatas.

%% Test cases

all_tests(Client) ->
    [?_test(test_auth_faied(Client)),
     ?_test(test_put_binary(Client)),
     ?_test(test_put_iolist(Client)),
     ?_test(test_put_stream_file(Client)),
     ?_test(test_put_stream_fun(Client)),
     ?_test(test_put_with_extra_headers(Client)),
     ?_test(test_put_unescaped_path(Client)),
     ?_test(test_put_escaped_path(Client)),
     ?_test(test_put_space_path(Client)),
     ?_test(test_put_chinese_path(Client)),
     ?_test(test_resume_put(Client)),
     ?_test(test_get_not_exist(Client)),
     ?_test(test_get_binary(Client)),
     ?_test(test_get_with_metadata(Client)),
     ?_test(test_get_stream_file(Client)),
     ?_test(test_get_stream_fun(Client)),
     ?_test(test_delete_not_exist(Client)),
     ?_test(test_delete(Client)),
     ?_test(test_async_delete(Client)),
     ?_test(test_getinfo_not_exist(Client)),
     ?_test(test_getinfo_file(Client)),
     ?_test(test_getinfo_directory(Client)),
     ?_test(test_mkdir(Client)),
     ?_test(test_rmdir_not_exist(Client)),
     ?_test(test_rmdir(Client)),
     ?_test(test_getlist(Client)),
     ?_test(test_usage(Client)),
     ?_test(test_metadata(Client)),
     ?_test(test_secret(Client)),
     ?_test(test_purge(Client))].

test_auth_faied(_Client) ->
    Options = [{bucket, <<"wrong-bucket">>},
               {operator, <<"wrong-operator">>},
               {password, <<"wrong-password">>}],
    {ok, Client} = upyun:init(Options),
    {error, {401, _}} = upyun:getinfo(Client, <<"/">>).

test_put_binary(Client) ->
    Key = gen_key(),
    FileBin = <<"file content - binary">>,
    ok = upyun:put(Client, Key, FileBin),
    {ok, FileBin} = upyun:get(Client, Key),
    %% Put exist key
    ok = upyun:put(Client, Key, <<>>),
    ok = upyun:delete(Client, Key).

test_put_iolist(Client) ->
    Key = gen_key(),
    FileIolist = ["file content ",<<"put iolist">>],
    ok = upyun:put(Client, Key, FileIolist),
    {ok, FileBin} = upyun:get(Client, Key),
    ?assertEqual(iolist_size(FileIolist), byte_size(FileBin)),
    ok = upyun:delete(Client, Key).

test_put_stream_file(Client) ->
    Key = gen_key(),
    FileSize = filelib:file_size(?FILE_NAME),
    ok = upyun:put(Client, Key, {stream, {file, ?FILE_NAME}}),
    {ok, FileBin} = upyun:get(Client, Key),
    ?assertEqual(FileSize, byte_size(FileBin)),
    ok = upyun:delete(Client, Key).

test_put_stream_fun(Client) ->
    Key = gen_key(),
    FileSize = filelib:file_size(?FILE_NAME),
    {ok, IoDevice} = file:open(?FILE_NAME, [read, binary]),
    StreamFun = fun() ->
                        case file:read(IoDevice, 4096) of
                            {ok, Data} -> {ok, Data};
                            eof        -> eof;
                            Error      -> Error
                        end
                end,
    ok = upyun:put(Client, Key, {stream, StreamFun}),
    file:close(IoDevice),
    {ok, FileBin} = upyun:get(Client, Key),
    ?assertEqual(FileSize, byte_size(FileBin)),
    ok = upyun:delete(Client, Key).

test_put_with_extra_headers(Client) ->
    Key = gen_key(),
    Headers = [{<<"content-type">>, <<"image/jpeg">>},
               {<<"x-gmkerl-type">>, <<"fix_width">>},
               {<<"x-gmkerl-value">>, <<"42">>},
               {<<"x-gmkerl-unsharp">>, <<"true">>}],
    ExpectedMetas = {42, 28, <<"JPEG">>, 1},
    {ok, Metas} = upyun:put(Client, Key, {file, ?FILE_NAME}, [{headers, Headers}]),
    ?assertEqual(ExpectedMetas, Metas).

test_put_unescaped_path(Client) ->
    Key = <<"/tests/~_-.:@+,;=">>,
    FileBin = <<"file content: unescaped">>,
    ok = upyun:put(Client, Key, FileBin),
    {ok, FileBin} = upyun:get(Client, Key),
    ok = upyun:delete(Client, Key).

test_put_escaped_path(Client) ->
    Key = <<"/tests/?#<>()[]{}!$&'*%`^">>,
    Key2 = <<"/tests/%3f%23%3c%3e%28%29%5b%5d%7b%7d%21%24%26%27%2a%25%60%5e">>,
    FileBin = <<"file content: escaped">>,
    ok = upyun:put(Client, Key, FileBin),
    {ok, FileBin} = upyun:get(Client, Key),
    {ok, FileBin} = upyun:get(Client, Key2),
    ok = upyun:delete(Client, Key).

test_put_space_path(Client) ->
    Key = <<"/tests/ file">>,
    Key2 = <<"/tests/+file">>,
    FileBin = <<"file content:  ">>,
    ok = upyun:put(Client, Key, FileBin),
    {ok, FileBin} = upyun:get(Client, Key),
    {ok, FileBin} = upyun:get(Client, Key2),
    ok = upyun:delete(Client, Key).

test_put_chinese_path(Client) ->
    Key = <<"/tests/这是中文路径"/utf8>>,
    Key2 = <<"/tests/%E8%BF%99%E6%98%AF%E4%B8%AD%E6%96%87%E8%B7%AF%E5%BE%84">>,
    FileBin = <<"file content: 这是中文路径"/utf8>>,
    ok = upyun:put(Client, Key, FileBin),
    {ok, FileBin} = upyun:get(Client, Key),
    {ok, FileBin} = upyun:get(Client, Key2),
    ok = upyun:delete(Client, Key).

test_resume_put(Client) ->
    Key = gen_key(),
    {ok, FileBin} = file:read_file(?FILE_NAME),
    ok = upyun:put(Client, Key, FileBin, [{resume, true}, {type, ?FILE_TYPE}]),
    {ok, FileBin} = upyun:get(Client, Key),
    ok = upyun:delete(Client, Key).

test_get_not_exist(Client) ->
    {error, notfound} = upyun:get(Client, <<"/tests/notfound">>).

test_get_binary(Client) ->
    Key = gen_key(),
    FileBin = <<"file content - get_binary">>,
    ok = upyun:put(Client, Key, FileBin),
    {ok, FileBin} = upyun:get(Client, Key),
    ok = upyun:delete(Client, Key).

test_get_with_metadata(Client) ->
    Key = gen_key(),
    FileBin = <<"file content - get_binary">>,
    ok = upyun:put(Client, Key, FileBin, [{headers, [{<<"x-upyun-meta-a">>, 1}]}]),
    {ok, {FileBin, Metadatas}} = upyun:get(Client, Key, [{with_metadata, true}]),
    ?assertEqual([{<<"a">>, <<"1">>}], Metadatas),
    ok = upyun:delete(Client, Key).

test_get_stream_file(Client) ->
    Key = gen_key(),
    FileBin = <<"file content - get_stream_file">>,
    FileName = "./test/tmp_file",
    ok = upyun:put(Client, Key, FileBin),
    ok = upyun:get(Client, Key, [{stream, {file, FileName}}]),
    {ok, FileBin} = file:read_file(FileName),
    ok = file:delete(FileName),
    ok = upyun:delete(Client, Key).

test_get_stream_fun(Client) ->
    Key = gen_key(),
    FileBin = <<"file content - get_stream_fun">>,
    FileName = "./test/tmp_file",
    ok = upyun:put(Client, Key, FileBin),
    {ok, IoDevice} = file:open(FileName, [write, binary]),
    StreamFun = fun({ok, Data}) ->
                        ok = file:write(IoDevice, Data);
                   (eof) ->
                        ok;
                   (_Error) ->
                        error
                end,
    ok = upyun:get(Client, Key, [{stream, StreamFun}]),
    file:close(IoDevice),
    {ok, FileBin} = file:read_file(FileName),
    ok = file:delete(FileName),
    ok = upyun:delete(Client, Key).

test_delete_not_exist(Client) ->
    {error, notfound} = upyun:delete(Client, <<"/tests/notfound">>).

test_delete(Client) ->
    Key = gen_key(),
    FileBin = <<"file content - delete">>,
    ok = upyun:put(Client, Key, FileBin),
    ok = upyun:delete(Client, Key),
    {error, notfound} = upyun:get(Client, Key).

test_async_delete(Client) ->
    Key = gen_key(),
    FileBin = <<"file content - async delete">>,
    ok = upyun:put(Client, Key, FileBin),
    ok = upyun:delete(Client, Key, true),
    timer:sleep(50),
    {error, notfound} = upyun:get(Client, Key).

test_getinfo_not_exist(Client) ->
    {error, notfound} = upyun:get(Client, <<"/tests/notfound">>).

test_getinfo_file(Client) ->
    Key = gen_key(),
    FileBin = <<"file content - getinfo file">>,
    ok = upyun:put(Client, Key, FileBin),
    {ok, {Type, Size, _LastModified}} = upyun:getinfo(Client, Key),
    ?assertEqual(file, Type),
    ?assertEqual(byte_size(FileBin), Size),
    ok = upyun:delete(Client, Key).

test_getinfo_directory(Client) ->
    ok = upyun:mkdir(Client, <<"/test_dir">>),
    {ok, {Type, Size, _LastModified}} = upyun:getinfo(Client, <<"/test_dir">>),
    ?assertEqual(directory, Type),
    ?assertEqual(0, Size),
    ok = upyun:rmdir(Client, <<"/test_dir">>).

test_mkdir(Client) ->
    ok = upyun:mkdir(Client, <<"/test_dir">>),
    %% Put already exist directory
    ok = upyun:mkdir(Client, <<"/test_dir">>),
    ok = upyun:rmdir(Client, <<"/test_dir">>).

test_rmdir_not_exist(Client) ->
    {error, notfound} = upyun:rmdir(Client, <<"/test_dir">>).

test_rmdir(Client) ->
    FileBin = <<"file content - rmdir">>,
    FileKey = <<"/test_dir/file">>,
    ok = upyun:put(Client, FileKey, FileBin),
    {error, {403, _, _}} = upyun:rmdir(Client, <<"/test_dir">>),
    ok = upyun:delete(Client, FileKey),
    ok = upyun:rmdir(Client, <<"/test_dir">>),
    %% Delete not exist directory
    {error, notfound} = upyun:rmdir(Client, <<"/test_dir">>).

test_getlist(Client) ->
    %% List not exist directory
    {error, notfound} = upyun:getlist(Client, <<"/tests/dir/">>, []),

    Dir = <<"/tests/">>,
    %% Put 12 files
    lists:foreach(fun(I) ->
                          Key0 = <<"/tests/file", (upyun_util:to_binary(I))/binary>>,
                          ok = upyun:put(Client, Key0, <<>>)
                  end, lists:seq(11, 22)),
    timer:sleep(10),

    %% Basic test
    Options1 = [{limit, 10}, {order, desc}],
    {ok, {Result1, Iter1}} = upyun:getlist(Client, Dir, Options1),
    ?assertEqual(10, length(Result1)),
    [[{name,<<"file22">>},{type,file},{size,0},{last_modified,_}] | _] = Result1,
    Options2 = [{limit, 10}, {order, desc}, {iter, Iter1}],
    {ok, {Result2, eof}} = upyun:getlist(Client, Dir, Options2),
    ?assertEqual(2, length(Result2)),
    Options3 = [{limit, 11}, {order, asc}, {iter, Iter1}],
    {ok, {Result3, eof}} = upyun:getlist(Client, Dir, Options3),
    ?assertEqual(11, length(Result3)),

    %% Test after delete 6 files
    lists:foreach(fun(I) ->
                          Key0 = <<"/tests/file", (upyun_util:to_binary(I))/binary>>,
                          ok = upyun:delete(Client, Key0)
                  end, lists:seq(11, 16)),
    timer:sleep(10),
    Options4 = [{limit, 6}, {order, asc}, {iter, undefined}],
    {ok, {Result4, eof}} = upyun:getlist(Client, Dir, Options4),
    ?assertEqual(6, length(Result4)),

    %% Test after delete all files
    lists:foreach(fun(I) ->
                          Key0 = <<"/tests/file", (upyun_util:to_binary(I))/binary>>,
                          ok = upyun:delete(Client, Key0)
                  end, lists:seq(17, 22)),
    timer:sleep(10),
    Options5 = [{limit, 10}, {order, asc}, {iter, undefined}],
    {ok, {[], eof}} = upyun:getlist(Client, Dir, Options5).

test_usage(Client) ->
    {ok, Result} = upyun:usage(Client),
    ?assert(Result >= 0).

test_metadata(Client) ->
    Key = gen_key(),
    %% Update metadata for not exist file
    {error, notfound} = upyun:metadata(Client, Key, [{<<"A">>, <<"1">>}]),
    %% After putting file
    FileBin = <<"file content - metadata">>,
    ok = upyun:put(Client, Key, FileBin),
    {ok, {_, _, LastModified}} = upyun:getinfo(Client, Key),
    %% Merge metadata
    ok = upyun:metadata(Client, Key, [{<<"A">>, <<"1">>}]),
    [{<<"A">>, <<"1">>}] = get_metadata(Client, Key),
    ok = upyun:metadata(Client, Key, [{<<"A">>, <<"2">>}]),
    [{<<"A">>, <<"2">>}] = get_metadata(Client, Key),
    %% Replace metadata
    ok = upyun:metadata(Client, Key, replace, [{<<"B">>, <<"2">>}]),
    [{<<"B">>, <<"2">>}] = get_metadata(Client, Key),
    %% Delete metadata & update `last_modified`
    timer:sleep(10),
    ok = upyun:metadata(Client, Key, delete, [{<<"B">>, <<"true">>}], true),
    [] = get_metadata(Client, Key),
    %% Also should have larger `last_modified`
    {ok, {_, _, LastModified2}} = upyun:getinfo(Client, Key),
    ?assert(LastModified2 > LastModified),
    ok = upyun:delete(Client, Key).

test_secret(Client) ->
    Key = gen_key(),
    FileBin = <<"file content - secret">>,
    Secret = <<"secret">>,
    ok = upyun:put(Client, Key, FileBin, [{secret, Secret}]),
    Url = << "http://", ?BUCKET/binary, ".b0.upaiyun.com", Key/binary >>,
    {ok, 404, _, _} = hackney:get(Url),
    {ok, 200, _, _} = hackney:get(<< Url/binary, $!, Secret/binary >>),
    ok = upyun:metadata(Client, Key, delete, [{<<"Secret">>, true}]),
    {ok, FileBin} = upyun:get(Client, Key),
    ok = upyun:delete(Client, Key).

test_purge(Client) ->
    ExpectedBody = <<"{\"invalid_domain_of_url\":{}}">>,
    {ok, ExpectedBody} = upyun:purge(Client, [<<"/tests/image.jpg">>]),
    {ok, ResBody} = upyun:purge(Client, <<"x.x-upyun.com">>, [<<"/test.txt">>]),
    ErrorBody = <<"{\"invalid_domain_of_url\":[\"http:\\/\\/x.x-upyun.com\\/\\/test.txt\"]}">>,
    ?assertEqual(ErrorBody, ResBody).
