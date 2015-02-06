-module(upyun_test).

-include_lib("eunit/include/eunit.hrl").

start_test() ->
  application:start(upyun).

init_test() ->
  {ok, {state, Bucket, Operator, PassWord, "v0.api.upyun.com", Status, 600000}} = upyun:init("travis", "travisci", "testtest", v0, 600000),
  ?assert(Bucket =:= "travis"),
  ?assert(Operator =:= "travisci"),
  ?assert(PassWord =:= "testtest"),
  ?assert(Status =:= inited).

getUage_test() ->
  {{_, Code, Status}, _, _} = upyun:getUage(),
  ?assert(Code =:= 200),
  ?assert(Status =:= "OK").

createDir_test() ->
  {{_, Code, Status}, _, _} = upyun:createDir("/travis/erlang-sdk/"),
  ?assert(Code =:= 200),
  ?assert(Status =:= "OK").

uploadFile_test() ->
  {{_, Code, Status}, _, _} = upyun:uploadFile("/travis/erlang-sdk/test.png", "../test/file/test.png", []),
  ?assert(Code =:= 200),
  ?assert(Status =:= "OK").

listDir_test() ->
  {{_, Code, Status}, _, _} = upyun:listDir("/travis/erlang-sdk/"),
  ?assert(Code =:= 200),
  ?assert(Status =:= "OK").

infoFile_test() ->
  {{_, Code, Status}, _, _} = upyun:listDir("/travis/erlang-sdk/test.png"),
  ?assert(Code =:= 200),
  ?assert(Status =:= "OK").

downloadFile_test() ->
  {Ok, Done} = upyun:downloadFile("/travis/erlang-sdk/test.png", "/tmp/test.png"),
  ?assert(Ok =:= ok),
  ?assert(Done =:= done).

removeFile_test() ->
  {{_, Code, Status}, _, _} = upyun:removeFile("/travis/erlang-sdk/test.png"),
  ?assert(Code =:= 200),
  ?assert(Status =:= "OK").

removeDir_test() ->
  {{_, Code, Status}, _, _} = upyun:removeDir("/travis/erlang-sdk/"),
  ?assert(Code =:= 200),
  ?assert(Status =:= "OK").
