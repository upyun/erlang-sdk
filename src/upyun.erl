-module(upyun).

-export([
  info/0,
  init/3,
  init/4,
  init/5,
  getUage/0,
  listDir/0,
  listDir/1,
  createDir/1,
  removeDir/1,
  uploadFile/2,
  uploadFile/3,
  infoFile/1,
  downloadFile/1,
  downloadFile/2,
  removeFile/1
]).

%% 用于获取初始化信息
info() ->
  up_core:get_info().

%% 默认线路为自动选择线路, 默认超时时间为60秒 即60000 毫秒
init(Bucket, Operator, PassWord) ->
  up_core:upyun_init(Bucket, Operator, PassWord, v0, 60000).

init(Bucket, Operator, PassWord, Line) ->
  up_core:upyun_init(Bucket, Operator, PassWord, Line, 60000).

init(Bucket, Operator, PassWord, Line, TimeOut) ->
  up_core:upyun_init(Bucket, Operator, PassWord, Line, TimeOut).

%% getUage() -> {Res_status, Res_head, Res_body} || {error, Reason}
getUage() ->
  up_core:getUsage().

listDir() ->
  up_core:listDir().

listDir(Path) ->
  up_core:listDir(Path).

createDir(Path) ->
  up_core:createDir(Path).

removeDir(Path) ->
  up_core:removeDir(Path).

uploadFile(File, LocalFile) ->
  up_core:uploadFile(File, LocalFile, []).

uploadFile(File, LocalFile, Opts) ->
  up_core:uploadFile(File, LocalFile, Opts).

infoFile(File) ->
  up_core:infoFile(File).

downloadFile(File) ->
  up_core:downloadFile(File).

downloadFile(File, SavePath) ->
  up_core:downloadFile(File, SavePath).

removeFile(File) ->
  up_core:removeFile(File).
