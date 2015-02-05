-module(up_core).
-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").

%% API
-export([
  start_link/0,
  stop/0,
  get_info/0,
  upyun_init/5,
  getUsage/0,
  listDir/0,
  listDir/1,
  createDir/1,
  removeDir/1,
  uploadFile/3,
  infoFile/1,
  downloadFile/1,
  downloadFile/2,
  removeFile/1
  ]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 10000).
-define(BLOCKSIZE, 32768).

-record(state, {bucket, operator, password, line, status, timeout}).



start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_info() ->
  gen_server:call(?SERVER, get_info).

get_status() ->
  gen_server:call(?SERVER, get_status).

upyun_init(Bucket, Operator, PassWord, Line, TimeOut) ->
  gen_server:call(?SERVER, {upyun_init, Bucket, Operator, PassWord, prefix_line(Line), TimeOut}).

stop() ->
  gen_server:cast(?SERVER, stop).


init([]) ->
  {ok, #state{line = v0, status = need_init, timeout = ?TIMEOUT}}.

handle_call({upyun_init, Bucket, Operator, PassWord, Line, TimeOut}, _From, State) ->
  NewState = State#state{bucket = Bucket, operator = Operator, password = PassWord, line = Line, status = inited, timeout = TimeOut},
  {reply, {ok, NewState}, NewState};

handle_call(get_info, _From, State) ->
  {reply, {ok, {State#state.bucket, State#state.operator, State#state.password, State#state.line, State#state.timeout}}, State};

handle_call(get_status, _From, State) ->
  {reply, {ok, State#state.status}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

prefix_line(Line) ->
  case Line of
    v0 -> "v0.api.upyun.com";
    auto -> "v0.api.upyun.com";
    v1 -> "v1.api.upyun.com";
    ctcc -> "v1.api.upyun.com";
    v2 -> "v2.api.upyun.com";
    cucc -> "v2.api.upyun.com";
    v3 -> "v3.api.upyun.com";
    cmcc -> "v3.api.upyun.com";
    default -> "v0.api.upyun.com";
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

ensure_inited() ->
  case get_status() of
    {ok, inited} -> ok;
    {ok, need_init} -> error
  end.

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

parse_res_body(S) ->
  lists:map(fun(X)->string:tokens(X, "\t") end, string:tokens(S, "\n")).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% open api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getUsage() ->
  case ensure_inited() of
    ok ->
      {ok, {Bucket, Operator, PassWord, Line, TimeOut}} = get_info(),
      URL = prefix_http(Line) ++ "/" ++ Bucket ++ "/?usage",
      Path = "/" ++ Bucket ++ "/",
      {Signature, Date} = signature("GET", Path, 0, Operator, PassWord ),
      Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", Line}, {"Content-Length", "0"}],
      ensure_start(inets),
      {ok, RequestId} = httpc:request(get, {URL, Header}, [], [{sync, false}]),
      receive
        {http, {RequestId, Result}} ->
          {Res_status, Res_head, Res_body} = Result,
          {Res_status, Res_head, binary_to_list(Res_body)}
        after TimeOut -> {error, timeout}
      end;
    error ->
      {error, need_init}
  end.
  

listDir() ->
  %% 无参数，获取当前空间的全部信息
  %% 等同于 listDir("/mytestbucket/")
  case ensure_inited() of
    ok ->
      {ok, {Bucket, Operator, PassWord, Line, TimeOut}} = get_info(),
      Path = "/" ++ Bucket ++ "/",
      URL = prefix_http(Line) ++ "/" ++ Bucket ++ "/",
      {Signature, Date} = signature("GET", Path, 0, Operator, PassWord),
      Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", Line}, {"Content-Length", "0"}],
      ensure_start(inets),
      {ok, RequestId} = httpc:request(get, {URL, Header}, [], [{sync, false}]),
      receive
        {http, {RequestId, Result}} ->
          {Res_status, Res_head, Res_body} = Result,
          {Res_status, Res_head, parse_res_body(binary_to_list(Res_body))}
        after TimeOut -> {error, timeout}
      end;
    error ->
      {error, ensure_init}
  end.


listDir(Path) ->
  %% eg: Path "/mytestbuckte/test/"
  case ensure_inited() of
    ok ->
      {ok, {_, Operator, PassWord, Line, TimeOut}} = get_info(),
      URL = prefix_http(Line) ++ Path,
      {Signature, Date} = signature("GET", Path, 0, Operator, PassWord),
      Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", Line}, {"Content-Length", "0"}],
      ensure_start(inets),
      {ok, RequestId} = httpc:request(get, {URL, Header}, [], [{sync, false}]),
      receive
        {http, {RequestId, Result}} ->
          {Res_status, Res_head, Res_body} = Result,
          {Res_status, Res_head, parse_res_body(binary_to_list(Res_body))}
        after TimeOut -> {error, timeout}
      end;
    error ->
      {error, need_init}
  end.


createDir(Path) ->
  %% eg: Path  "/mytestbucket/testdir/"
  case ensure_inited() of
    ok ->
      {ok, {_, Operator, PassWord, Line, TimeOut}} = get_info(),
      URL = prefix_http(Line) ++ Path,
      {Signature, Date} = signature("POST", Path, 0, Operator, PassWord),
      Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", Line}, {"Content-Length", "0"}, {"Folder", "true"}],
      ensure_start(inets),
      {ok, RequestId} = httpc:request(post, {URL, Header, [], [] }, [], [{sync, false}]),
      receive
        {http, {RequestId, Result}} ->
          {Res_status, Res_head, Res_body} = Result,
          {Res_status, Res_head, binary_to_list(Res_body)};
        {http, {RequestId, {error, Reason}}} ->
          {error, Reason}
        after TimeOut -> { error, timeout}
      end;
    error ->
      {error, need_init}
  end.


removeDir(Path) ->
  %% eg: Path  "/mytestbucket/testdir/"
  case ensure_inited() of
    ok ->
      {ok, {_, Operator, PassWord, Line, TimeOut}} = get_info(),
      URL = prefix_http(Line) ++ Path,
      {Signature, Date} = signature("DELETE", Path, 0, Operator, PassWord),
      Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", Line}, {"Content-Length", "0"}],
      ensure_start(inets),
      {ok, RequestId} = httpc:request(delete, {URL, Header, [], []}, [], [{sync, false}]),
      receive
        {http, {RequestId, Result}} ->
          {Res_status, Res_head, Res_body} = Result,
          {Res_status, Res_head, binary_to_list(Res_body)};
        {http, {RequestId, {error, Reason}}} ->
          {error, Reason}
      after TimeOut -> { error, timeout}
    end;
    error ->
      {error, need_init}
  end.


uploadFile(File, LocalFile, Opts) ->
  %% eg: File "/mytestbucket/test/b.txt"
  %% eg: LocalFile "../upyun.erl"
  case ensure_inited() of
    ok ->
      {ok, {_, Operator, PassWord, Line, TimeOut}} = get_info(),
      URL = prefix_http(Line) ++ File,
      Path = File,
      case file:read_file_info(LocalFile) of
        {ok, Info} ->
          Size = Info#file_info.size,
          %Type = Info#file_info.type,
          {ok, Bin } = file:read_file(LocalFile),
          Body = binary_to_list(Bin),
          {Signature, Date} = signature("PUT", Path, Size, Operator, PassWord),
          Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", Line}, {"Content-Length", integer_to_list(Size)},
          {"Content-MD5", md5_file(File)}] ++ Opts,
          ensure_start(inets),
          {ok, RequestId} = httpc:request(put, {URL, Header, [], Body}, [], [{sync, false}]),
          receive
            {http, {RequestId, Result}} ->
              {Res_status, Res_head, Res_body} = Result,
              {Res_status, Res_head, binary_to_list(Res_body)};
            {http, {RequestId, {error, Reason}}} ->
              {error, Reason}
            after TimeOut -> {error, timeout}
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    error ->
      {error, need_init}
    end.


infoFile(File) ->
  %% eg: File "/mytestbucket/test/a.txt"
  case ensure_inited() of
    ok ->
      {ok, {_, Operator, PassWord, Line, TimeOut}} = get_info(),
      URL = prefix_http(Line) ++ File,
      Path = File,
      {Signature, Date} = signature("HEAD", Path, 0, Operator, PassWord ),
      Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", Line}, {"Content-Length", "0"}],
      ensure_start(inets),
      {ok, RequestId} = httpc:request(head, {URL, Header}, [], [{sync, false}]),
      receive
        {http, {RequestId, Result}} ->
          {Res_status, Res_head, Res_body} = Result,
          {Res_status, Res_head, binary_to_list(Res_body)}
        after TimeOut -> {error, timeout}
      end;
    error ->
      {error, need_init}
  end.


downloadFile(File) ->
  %% eg: File "/mytestbucket/test/a.txt"
  case ensure_inited() of
    ok ->
      {ok, {_, Operator, PassWord, Line, TimeOut}} = get_info(),
      URL = prefix_http(Line) ++ File,
      Path = File,
      {Signature, Date} = signature("GET", Path, 0, Operator, PassWord ),
      Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", Line}, {"Content-Length", "0"}],
      ensure_start(inets),
      {ok, RequestId} = httpc:request(get, {URL, Header}, [], [{sync, false}]),
      receive
        {http, {RequestId, Result}} ->
          {Res_status, Res_head, Res_body} = Result,
          {Res_status, Res_head, binary_to_list(Res_body)}
        after TimeOut -> {error, timeout}
      end;
    error ->
      {error, need_init}
  end.


downloadFile(File, SavePath) ->
  %% eg: "File /mytestbucket/test/a.txt"
  case ensure_inited() of
    ok ->
      {ok, {_, Operator, PassWord, Line, TimeOut}} = get_info(),
      URL = prefix_http(Line) ++ File,
      Path = File,
      {Signature, Date} = signature("GET", Path, 0, Operator, PassWord ),
      Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", Line}, {"Content-Length", "0"}],
      ensure_start(inets),
      {ok, RequestId} = httpc:request(get, {URL, Header}, [], [{sync, false}]),
      receive
        {http, {RequestId, Result}} ->
          {_, _, Res_body} = Result,
          case file:write_file(SavePath, Res_body) of
            ok -> {ok, done};
            {error, Reason} -> {error, Reason}
          end
        after TimeOut -> {error, timeout}
      end;
    error ->
      {error, need_init}
  end.

removeFile(File) ->
  %% eg: File  "/mytestbucket/test/a.txt"
  case ensure_inited() of
    ok ->
      {ok, {_, Operator, PassWord, Line, TimeOut}} = get_info(),
      URL = prefix_http(Line) ++ File,
      Path = File,
      {Signature, Date} = signature("DELETE", Path, 0, Operator, PassWord),
      Header = [{"Authorization", Signature}, {"Date", Date}, {"Host", Line}, {"Content-Length", "0"}],
      ensure_start(inets),
      {ok, RequestId} = httpc:request(delete, {URL, Header, [], []}, [], [{sync, false}]),
      receive
        {http, {RequestId, Result}} ->
          {Res_status, Res_head, Res_body} = Result,
          {Res_status, Res_head, binary_to_list(Res_body)};
        {http, {RequestId, {error, Reason}}} ->
          {error, Reason}
        after TimeOut -> {error, timeout}
      end;
    error ->
      {error, need_init}
  end.
