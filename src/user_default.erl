%% @author
%% @doc 常用命令模块
%% Created 2013-2-20


-module(user_default).
-include("ets_name.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

%%在线人数
online_num() ->
  ets:info(?ets_role_online, size).

online_random_one() ->
  case ets:first(?ets_role_online) of
    '$end_of_table' ->
      0;
    ID ->
      ID
  end.

online_list() ->
  ets:tab2list(?ets_role_online).

%%服务进程字典
%%dic(role2305843009213693953, socket).
dic(P, Key) ->
  gen_server:call(p(P), {dic, Key}).

%%服务状态表
%%state(role2305843009213693953).
state(P) ->
  gen_server:call(p(P), state).

%%打印ets表
ets_dump(Tab) ->
  ets:tab2list(Tab).

online_role_sup() ->
  supervisor:count_children(role_sup).

app(List) ->
  main:start_applications(List).

emu(PID, Msg) when is_pid(PID) ->
  erlang:send(PID, {emu, Msg});
emu(SevName, Msg) when is_atom(SevName) ->
  erlang:send(SevName, {emu, Msg});
emu(RoleID, Msg) when is_integer(RoleID) ->
  erlang:send(role_lib:gatewayRegName(RoleID), {emu, Msg}).

%%请求玩家服务
callserver(RoleID, M, F, A) ->
  role_lib:call_server(RoleID, {func, M, F, A}).

start() ->
  main:start().
s() ->
  start().

stop() ->
  %%    ?TRY_CATCH(broadcast_server:rolling_msg(xmerl_ucs:to_utf8("服务器将于5秒之后进行重启,请耐心等待重启~") )),
  timer:sleep(1000 * 5),
  main:stop().
quit() ->
  stop().

%% 重新加载全部配置
lc() ->
  config:preload_config(),
  reload().
lc(A) ->
  Result = config:reload_config(A),
  Result.

%% 重新生成协议
pg() ->
  %%    todo
  RootDir = config:root_dir(),
  proto_compile:scan_dir(filename:join([RootDir, "proto"]), filename:join([RootDir, ""])).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc 清除所有历史日志
cl() ->
  Dir = data_setting:get(logger_file_dir),
  filelib:fold_files(Dir, ".+", false, fun(E, _) -> io:format("~s\n", [E]),
    ok = file:delete(E) end, ok).

%%打印进程pid
p(Name) when is_atom(Name) ->
  whereis(Name);
p(RoleID) when is_integer(RoleID) ->
  whereis(role_lib:regName(RoleID));
p(Pid) when is_pid(Pid) ->
  Pid.

%%打印进程字典
d(P) ->
  element(2, process_info(p(P), dictionary)).

d(P, Key) ->
  Dict = element(2, process_info(p(P), dictionary)),
  case lists:keyfind(Key, 1, Dict) of
    false ->
      undefined;
    {_, Value} ->
      Value
  end.

d2(P, Key2) ->
  lists:foreach(fun({{Kk, _}, _} = V) when Key2 == Kk ->
    io:format("~w~n", [V]);
    (_) -> ignore
  end, element(2, process_info(p(P), dictionary))).

put(P, Key, Value) ->
  erlang:send(p(P), {func, fun erlang:put/2, [Key, Value]}).

i(P) ->
  gen_server:call(p(P), i).

ri(P) ->
  d(P, roleInfo).

kill(P) ->
  exit(p(P), kill).

z() ->
  make:all(),
  reload(),
  ok.

lib_dir() ->
  {F, _Options} = filename:find_source(lists),
  filename:dirname(filename:dirname(filename:dirname(F))).

reload() ->
  LibDir = lib_dir(),
  %% 蛋疼的代码，在windows下，盘符，有时返回E:,有时返回e:
  [erlang:insert_element(2, c:l(E), E) || {E, P} <- code:all_loaded(), P =/= preloaded,
    {P2, _} <- [filename:find_source(E)],
    not(is_list(P2) andalso lists:prefix(LibDir, filename:dirname(P2)))].

compile_options(RootDir) ->
  EmakefileName = filename:join([RootDir, "Emakefile"]),
  {ok, [{_, CompileOptions} | _]} = file:consult(EmakefileName),
  lists:map(fun({i, Dir}) ->
    {i, filename:join([RootDir, Dir])};
    ({outdir, Dir}) ->
      {outdir, filename:join([RootDir, Dir])};
    (E) ->
      E
  end, CompileOptions).

find_file(Mod, Dir) ->
  FileName = atom_to_list(Mod) ++ ".erl",
  filelib:fold_files(Dir, FileName, true, fun(E, _Acc) -> E end, undefined).

compile(Mod, RootDir) when is_atom(Mod) ->
  File = find_file(Mod, RootDir),
  compile:file(File, compile_options(RootDir)).

%%erlang console中的热加载编译更新
r(Mod) ->
  compile(Mod, config:root_dir()),
  c:l(Mod).

cc() ->
  r(user_default).

%%编译并加载模块
rc(ModeName) ->
  FileName = erlang:atom_to_list(ModeName) ++ ".erl",
  FileNameWithPath = filelib:fold_files(config:root_dir(), FileName, true, fun(F, _) ->
    F end, 0),
  case FileNameWithPath of
    0 ->
      io:format("module ~p can not finded~n", [ModeName]);
    _ ->
      {_, [{_, Options}]} = file:consult(filename:join([config:root_dir(), "Emakefile"])),
      TotalLength = length(FileNameWithPath),
      {FileNameWithoutAppend, _} = lists:split(TotalLength - 4, FileNameWithPath),
      ComPileRet = compile:file(FileNameWithoutAppend, Options),
      c:l(ModeName),
      io:format("module ~p ,loaded success,~nits path is ~p~nComPileRet ~p ~n", [ModeName, FileNameWithPath, ComPileRet])
  end.


mm() ->
  io:format("total:~wM~n", [erlang:trunc(erlang:memory(total) / 1000 / 1000)]),
  io:format("processes:~wM~n", [erlang:trunc(erlang:memory(processes) / 1000 / 1000)]),
  io:format("processes_used:~wM~n", [erlang:trunc(erlang:memory(processes_used) / 1000 / 1000)]).

gc() ->
  lists:foreach(fun(Pid) -> erlang:garbage_collect(Pid) end, erlang:processes()).

msort(Count) ->
  List = [begin {_, M} = erlang:process_info(P, memory),
  case erlang:process_info(P, registered_name) of
    [] -> N = "";
    {registered_name, N} -> N
  end,
  {N, erlang:trunc(M / 1000 / 1000)}
  end || P <- erlang:processes()],
  List2 = lists:sort(fun({_, M1}, {_, M2}) -> if M2 < M1 -> true;true -> false end end, List),
  List3 = lists:sublist(List2, 1, Count),
  lists:foreach(fun({N, M}) -> io:format("~s: ~w~n", [N, M]) end, lists:reverse(List3)).

%%%%测试连接远程
%%node_call_all(M,F,A) ->
%%    ets:foldl(fun(#r_node{serverNode = Node},Acc) ->
%%        rpc:call(Node,M,F,A,3000),
%%        Acc
%%              end,[],?ETS_NODE),
%%    ok.
%%
%%node_call_one(ServerID,M,F,A) ->
%%    case node_server:get_node_info(ServerID) of
%%        ?undefined ->
%%            io:format("nodes not get this serverID");
%%        Node ->
%%            rpc:call(Node,M,F,A,100000)
%%    end,
%%    ok.

async_stop() ->
  rpc:eval_everywhere([node()], user_default, stop, []).

async_all_stop() ->
  rpc:eval_everywhere(user_default, stop, []).
