%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% author: kaidi.ren@upai.com
%% version: 0.0.2
%% 示例里的空间名为 mytestbucket
%% 所有的请求请以 /mytestbucket/ 开头
%% 示例：空间根目录为 /mytestbucket/
%% 示例：空间根目录中的文件为 /mytestbucket/hi.erl
%% 示例: 次级目录为: /mytestbucket/test/
%% 示例: 次级目录中的文件为: /mytestbucket/test/hello.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(upyun).
-behaviour(gen_fsm). 
-include_lib("kernel/include/file.hrl").

%% 外部api 
-export([
  start_link/0,
  stop/0,
  info/0,
  get_info/0,
  upyun_init/1,
  uninit/3,
  inited/3
]).
 
%% fsm回调函数 
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]). 
 
-define(LIMIT, 4). 
-record(state, { param = [] }).

%% API 接口
-export([
  getUsage/0,
  listDir/0,
  listDir/1,
  createDir/1,
  removeDir/1,
  uploadFile/3,
  downloadFile/1,
  downloadFile/2,
  removeFile/1,
  getInfo/1
]).


start_link()->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([])->
  {ok, uninit, #state{}}. 

stop() ->
  gen_fsm:send_all_state_event(?MODULE, stop).

%% @spec info() -> {ok, Info} 
%% @doc 查看当前状态 
info() ->
  gen_fsm:sync_send_event(?MODULE, info).

get_info() ->
  gen_fsm:sync_send_all_state_event(?MODULE, {get_info}).

upyun_init({Bucket, Operator, PassWord, EndPoint}) ->
  ensure_start_link(upyun),
  gen_fsm:sync_send_all_state_event(?MODULE, {set_param, "Bucket", Bucket}),
  gen_fsm:sync_send_all_state_event(?MODULE, {set_param, "Operator", Operator}),
  gen_fsm:sync_send_all_state_event(?MODULE, {set_param, "PassWord", PassWord}),
  gen_fsm:sync_send_all_state_event(?MODULE, {set_param, "EndPoint", ed_point(EndPoint)}),
  {ok, inited}.
 
handle_event(stop, _StateName, State) ->
  {stop, normal, State};
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

%% 增加一个任务
handle_sync_event({set_param, ParamName, Heavy}, _From, uninit, State = #state{param = Param}) ->
  NewParam = case lists:keyfind(ParamName, 1, Param) of
    {ParamName, _} ->
      Param1 = lists:keydelete(ParamName, 1, Param),
      [{ParamName, Heavy} | Param1];
    false ->
      [{ParamName, Heavy} | Param]
  end,
  StateName = case length(NewParam) >= ?LIMIT of
    true -> inited;
    false -> uninit
  end,
  {reply, {ok, NewParam}, StateName, State#state{param = NewParam}};

handle_sync_event({set_param, _ParamName, _Heavy}, _From, StateName, State) ->
  {reply, {error, already_inited}, StateName, State};


handle_sync_event({get_info}, _From, inited, State = #state{param = Param}) ->
  StateName = case length(Param) < ?LIMIT of
    true -> uninit;
    false -> inited
  end,
  {_, Bucket} = lists:keyfind("Bucket", 1, Param),
  {_, Operator} = lists:keyfind("Operator", 1, Param),
  {_, PassWord} = lists:keyfind("PassWord", 1, Param),
  {_, EndPoint} = lists:keyfind("EndPoint", 1, Param),
  {reply, {ok, {Bucket, Operator, PassWord, EndPoint}}, StateName, State#state{param = Param}};

handle_sync_event({get_info}, _From, StateName, State) ->
  {reply, {error, need_init}, StateName, State};

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.
 
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) -> 
  ok.

code_change(_OldVsn, StateName, State, _Extra) -> 
  {ok, StateName, State}.


uninit(info, _From, State) ->
  {reply, {ok, uninit}, uninit, State}.
inited(info, _From, State) ->
  {reply, {ok, inited}, inited, State}.

ed_point(ED) ->
  case ED of
    v0 -> "v0.api.upyun.com";
    auto -> "v0.api.upyun.com";
    v1 -> "v1.api.upyun.com";
    ctcc -> "v1.api.upyun.com";
    v2 -> "v2.api.upyun.com";
    cucc -> "v2.api.upyun.com";
    v3 -> "v3.api.upyun.com";
    cmcc -> "v3.api.upyun.com";
    _ -> "v0.api.upyun.com"
  end.

day(D) ->
  case D of
    1 -> "Mon,";
    2 -> "Tue,";
    3 -> "Wed,";
    4 -> "Thu,";
    5 -> "Fri,";
    6 -> "Sat,";
    7 -> "Sun,"
  end.

month(M)  ->
  case M of
    1 -> "Jan";
    2  -> "Feb";
    3  -> "Mar";
    4  -> "Apr";
    5  -> "May";
    6  -> "Jun";
    7  -> "Jul";
    8  -> "Aug";
    9  -> "Sep";
    10 -> "Oct";
    11 -> "Nov";
    12 -> "Dec"
  end.

ensure_start(M) ->
  case M:start() of
    ok -> ok;
    {error, {already_started, _}} -> ok
  end.

ensure_start_link(M) ->
  case M:start_link() of
    {ok, _} -> ok;
    { error, { already_started, _} } -> ok
  end.

ensure_inited(M) ->
  ensure_start_link(upyun),
  case M:info() of
    {ok, inited} -> ok;
    {ok, uninit} -> error
  end.
    

-define(BLOCKSIZE, 32768).

file(File) ->
  case file:open(File, [binary, raw, read]) of
    {ok, P} -> loop(P, erlang:md5_init());
    Error -> Error
  end.

loop(P, C) ->
  case file:read(P, ?BLOCKSIZE) of
    { ok, Bin } ->
      loop(P, erlang:md5_update(C, Bin));
    eof ->
      file:close(P),
      { ok, erlang:md5_final(C) }
  end.

bin2str(Bin) ->
  binary_to_list(list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(Bin)])).

md5_str(S) ->
  bin2str(erlang:md5(S)).

md5_file(File) ->
  case file(File) of
    {ok, Bin} -> bin2str(Bin);
    {error, _} -> "error"
  end.

prefix(S) ->
  string:right(S,2,$0).

prefix_http(S) ->
  "http://" ++ S.

%prefix_https(S) ->
%  "https://" ++ S.

httpdate_rfc1123() ->
  {{Year, Month, Day},{Hour, Minute, Second}} = calendar:now_to_universal_time(erlang:now()),
  Week_Day_Str = day(calendar:day_of_the_week(Year, Month, Day)),
  Month_Str = month(Month),
  H_M_S = string:join([prefix(integer_to_list(Hour)), prefix(integer_to_list(Minute)), prefix(integer_to_list(Second))], ":"),
  Date = string:join([ Week_Day_Str, prefix(integer_to_list(Day)), Month_Str, integer_to_list(Year), H_M_S, "GMT"], " "),
  Date.

signature(Method, Path, Content_Length, Operator, PassWord) ->
  Date = httpdate_rfc1123(),
  L = string:join([[Method], [Path], [Date],[integer_to_list(Content_Length)], [md5_str(PassWord)]], ""),
  Sgin = md5_str(string:join(L, "&")),
  { "UpYun " ++ Operator ++ ":" ++ Sgin, Date }.


%get_couple(K, L) ->
%  [H|_] = [{ Key, Value } || { Key, Value } <- L, Key =:= K],
%  H.

getUsage() ->
  case ensure_inited(upyun) of
    ok -> ok,
    {ok, {Bucket, Operator, PassWord, EndPoint}} = get_info(),
    URL = prefix_http(EndPoint) ++ "/" ++ Bucket ++ "/?usage",
    Path = "/" ++ Bucket ++ "/",
    {Signature, Date} = signature("GET", Path, 0, Operator, PassWord ),
    Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", EndPoint}, {"Content-Length", "0"}],
    ensure_start(inets),
    {ok, RequestId} = httpc:request(get, {URL, Header}, [], [{sync, false}]),
    receive
      {http, {RequestId, Result}} ->
        {Res_status, Res_head, Res_body} = Result,
        {Res_status, Res_head, binary_to_list(Res_body)}
      after 1000 -> {error, "timeout 1000"}
    end;
    error ->
      {error, need_init}
  end.
  
parse_res_body(S) ->
  lists:map(fun(X)->string:tokens(X, "\t") end, string:tokens(S, "\n")).

listDir() ->
  %% 无参数，获取当前空间的全部信息
  %% 等同于 listDir("/mytestbucket/")
  case ensure_inited(upyun) of
    ok -> ok,
    {ok, {Bucket, Operator, PassWord, EndPoint}} = get_info(),
    Path = "/" ++ Bucket ++ "/",
    URL = prefix_http(EndPoint) ++ "/" ++ Bucket ++ "/",
    {Signature, Date} = signature("GET", Path, 0, Operator, PassWord),
    Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", EndPoint}, {"Content-Length", "0"}],
    ensure_start(inets),
    {ok, RequestId} = httpc:request(get, {URL, Header}, [], [{sync, false}]),
    receive
      {http, {RequestId, Result}} ->
        {Res_status, Res_head, Res_body} = Result,
        {Res_status, Res_head, parse_res_body(binary_to_list(Res_body))}
      after 1000 -> {error, "timeout 1000"}
    end;
    error ->
      {error, ensure_init}
  end.

listDir(Path) ->
  %% eg: Path "/mytestbuckte/test/"
  case ensure_inited(upyun) of
    ok -> ok,
    {ok, {_, Operator, PassWord, EndPoint}} = get_info(),
    URL = prefix_http(EndPoint) ++ Path,
    {Signature, Date} = signature("GET", Path, 0, Operator, PassWord),
    Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", EndPoint}, {"Content-Length", "0"}],
    ensure_start(inets),
    {ok, RequestId} = httpc:request(get, {URL, Header}, [], [{sync, false}]),
    receive
      {http, {RequestId, Result}} ->
        {Res_status, Res_head, Res_body} = Result,
        {Res_status, Res_head, parse_res_body(binary_to_list(Res_body))}
      after 1000 -> {error, "timeout 1000"}
    end;
    error ->
      {error, need_init}
  end.


createDir(Path) ->
  %% eg: Path  "/mytestbucket/testdir/"
  case ensure_inited(upyun) of
    ok -> ok,
    {ok, {_, Operator, PassWord, EndPoint}} = get_info(),
    URL = prefix_http(EndPoint) ++ Path,
    {Signature, Date} = signature("POST", Path, 0, Operator, PassWord),
    Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", EndPoint}, {"Content-Length", "0"}, {"Mkdir", "true"}, {"Folder", "true"}],
    ensure_start(inets),
    {ok, RequestId} = httpc:request(post, {URL, Header, [], [] }, [], [{sync, false}]),
    receive
      {http, {RequestId, Result}} ->
        {Res_status, Res_head, Res_body} = Result,
        {Res_status, Res_head, binary_to_list(Res_body)};
      {http, {RequestId, {error, Reason}}} ->
        {error, Reason}
      after 1000 -> "timeout 1000"
    end;
    error ->
      {error, need_init}
  end.

removeDir(Path) ->
  %% eg: Path  "/mytestbucket/testdir/"
  case ensure_inited(upyun) of
    ok ->
      {ok, {_, Operator, PassWord, EndPoint}} = get_info(),
      URL = prefix_http(EndPoint) ++ Path,
      {Signature, Date} = signature("DELETE", Path, 0, Operator, PassWord),
      Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", EndPoint}, {"Content-Length", "0"}],
      ensure_start(inets),
      {ok, RequestId} = httpc:request(delete, {URL, Header, [], []}, [], [{sync, false}]),
      receive
        {http, {RequestId, Result}} ->
          {Res_status, Res_head, Res_body} = Result,
          {Res_status, Res_head, binary_to_list(Res_body)};
        {http, {RequestId, {error, Reason}}} ->
          {error, Reason}
      after 1000 -> "timeout 1000"
    end;
    error ->
      {error, need_init}
  end.


uploadFile(File, LocalFile, Opts) ->
  %% eg: File "/mytestbucket/test/b.txt"
  %% eg: LocalFile "../upyun.erl"
  case ensure_inited(upyun) of
    ok ->
      {ok, {_, Operator, PassWord, EndPoint}} = get_info(),
      URL = prefix_http(EndPoint) ++ File,
      Path = File,
      case file:read_file_info(LocalFile) of
        {ok, Info} ->
          Size = Info#file_info.size,
          %Type = Info#file_info.type,
          {ok, Bin } = file:read_file(LocalFile),
          Body = binary_to_list(Bin),
          {Signature, Date} = signature("PUT", Path, Size, Operator, PassWord),
          Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", EndPoint}, {"Content-Length", integer_to_list(Size)},
          {"Content-MD5", md5_file(File)}] ++ Opts,
          ensure_start(inets),
          {ok, RequestId} = httpc:request(put, {URL, Header, [], Body}, [], [{sync, false}]),
          receive
            {http, {RequestId, Result}} ->
              {Res_status, Res_head, Res_body} = Result,
              {Res_status, Res_head, binary_to_list(Res_body)};
            {http, {RequestId, {error, Reason}}} ->
              {error, Reason}
            after 3000 -> "timeout 3000"
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    error ->
      {error, need_init}
    end.

getInfo(File) ->
  %% eg: File "/mytestbucket/test/a.txt"
  case ensure_inited(upyun) of
    ok ->
      {ok, {_, Operator, PassWord, EndPoint}} = get_info(),
      URL = prefix_http(EndPoint) ++ File,
      Path = File,
      {Signature, Date} = signature("HEAD", Path, 0, Operator, PassWord ),
      Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", EndPoint}, {"Content-Length", "0"}],
      ensure_start(inets),
      {ok, RequestId} = httpc:request(head, {URL, Header}, [], [{sync, false}]),
      receive
        {http, {RequestId, Result}} ->
          {Res_status, Res_head, Res_body} = Result,
          {Res_status, Res_head, binary_to_list(Res_body)}
        after 1000 -> {error, "timeout 1000"}
      end;
    error ->
      {error, need_init}
  end.



downloadFile(File) ->
  %% eg: File "/mytestbucket/test/a.txt"
  case ensure_inited(upyun) of
    ok ->
      {ok, {_, Operator, PassWord, EndPoint}} = get_info(),
      URL = prefix_http(EndPoint) ++ File,
      Path = File,
      {Signature, Date} = signature("GET", Path, 0, Operator, PassWord ),
      Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", EndPoint}, {"Content-Length", "0"}],
      ensure_start(inets),
      {ok, RequestId} = httpc:request(get, {URL, Header}, [], [{sync, false}]),
      receive
        {http, {RequestId, Result}} ->
          {Res_status, Res_head, Res_body} = Result,
          {Res_status, Res_head, binary_to_list(Res_body)}
        after 1000 -> {error, "timeout 1000"}
      end;
    error ->
      {error, need_init}
  end.


downloadFile(File, SavePath) ->
  %% eg: "File /mytestbucket/test/a.txt"
  case ensure_inited(upyun) of
    ok ->
      {ok, {_, Operator, PassWord, EndPoint}} = get_info(),
      URL = prefix_http(EndPoint) ++ File,
      Path = File,
      {Signature, Date} = signature("GET", Path, 0, Operator, PassWord ),
      Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", EndPoint}, {"Content-Length", "0"}],
      ensure_start(inets),
      {ok, RequestId} = httpc:request(get, {URL, Header}, [], [{sync, false}]),
      receive
        {http, {RequestId, Result}} ->
          {_, _, Res_body} = Result,
          case file:write_file(SavePath, Res_body) of
            ok -> {ok, done};
            {error, Reason} -> {error, Reason}
          end
        after 1000 -> {error, "timeout 1000"}
      end;
    error ->
      {error, need_init}
  end.

removeFile(File) ->
  %% eg: File  "/mytestbucket/test/a.txt"
  case ensure_inited(upyun) of
    ok ->
      {ok, {_, Operator, PassWord, EndPoint}} = get_info(),
      URL = prefix_http(EndPoint) ++ File,
      Path = File,
      {Signature, Date} = signature("DELETE", Path, 0, Operator, PassWord),
      Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", EndPoint}, {"Content-Length", "0"}],
      ensure_start(inets),
      {ok, RequestId} = httpc:request(delete, {URL, Header, [], []}, [], [{sync, false}]),
      receive
        {http, {RequestId, Result}} ->
          {Res_status, Res_head, Res_body} = Result,
          {Res_status, Res_head, binary_to_list(Res_body)};
        {http, {RequestId, {error, Reason}}} ->
          {error, Reason}
        after 1000 -> "timeout 1000"
      end;
    error ->
      {error, need_init}
  end.
