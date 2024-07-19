%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 6月 2022 11:01
%%%-------------------------------------------------------------------
-module(main).

-export([start/0, stop/0, kill_self/0]).

-define(APP, [
  sasl
  , asn1
  , crypto
  , public_key
  , ssl
  , inets
  , syntax_tools
  , compiler
  , goldrush
  , lager
  , ranch
  , gpb
  , config
  , mysql
  , poolboy
  , mysql_poolboy
  , db
  , role
  , gate %%put at last
]).

manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
  Iterate(fun(App, Acc) ->
    catch io:format("=================manage_applications, app=~w\n", [App]),
    case Do(App) of
      ok -> [App | Acc];
      {error, {SkipError, _}} -> Acc;
      {error, Reason} ->
        lists:foreach(Undo, Acc),
        throw({error, {ErrorTag, App, Reason}})
    end
          end, [], Apps),
  ok.

start_applications(Apps) ->
  manage_applications(fun lists:foldl/3,
    fun application:start/1,
    fun application:stop/1,
    already_started,
    cannot_start_application,
    Apps).

stop_applications(Apps) ->
  manage_applications(fun lists:foldr/3,
    fun application:stop/1,
    fun application:start/1,
    not_started,
    cannot_stop_application,
    Apps).

start1(Time, Fun) -> spawn(fun() -> timer(Time, Fun) end).
cancel1(Pid) -> Pid ! cancel.
timer(Time, Fun) ->
  receive
    cancel ->
      void
  after Time ->
    Fun()
  end.

start() ->
  start_applications(?APP),
  lager:info("server is started"),
  %% 启动后对虚拟机整体垃圾回收一次
  lists:foreach(fun(Pid) -> erlang:garbage_collect(Pid) end, erlang:processes()).

stop() ->
  lager:info("server is stopping"),
%%  start1(200, fun() -> lager:error("stop") end),
%%  start1(200, fun() -> lager:error("stop") end),
%%  timer:sleep(2*1000),
  %% 先t所有玩家,并关闭游戏入口
  application:stop(gate),
%%  catch io:format("is stopping gate~n"),
  %% 等待10秒，等玩家进程保存数据
%%  todo,
  timer:sleep(10 * 1000),
  application:stop(role),

  catch stop_applications([goldrush, lager, ranch, gpb, config, mysql, poolboy, mysql_poolboy, db, mnesia]),
  timer:sleep(3 * 1000),
  kill_self(),
  catch stop_applications([sasl, asn1, crypto, public_key, ssl, inets, syntax_tools, compiler]),
  timer:sleep(3 * 1000),
  erlang:halt().

%%获取自己的进程号
kill_self() ->
  case os:type() of
    {unix, _} ->
      CMD = io_lib:format("ps -ef|grep '~s'|head -1|awk '{printf $2}'", ["nam[e] " ++ util:to_string(node())]),
      LinuxPid = os:cmd(CMD),
      KillCMD = io_lib:format("kill ~s", [LinuxPid]),
      my_exec(KillCMD),
      catch io:format("is linux!!!!!"),
      io:format("system linux exist ok!");
    _ ->
      io:format("system windows exist ok!")
  end.


my_exec(Command) ->
  Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status]),
  Result = get_data(Port, []),
  Result.

get_data(Port, Sofar) ->
  receive
    {Port, {data, Bytes}} ->
      get_data(Port, [Sofar | Bytes]);
    {Port, eof} ->
      Port ! {self(), close},
      receive
        {Port, closed} ->
          true
      end,
      receive
        {'EXIT', Port, _} ->
          ok
      after 1 ->              % force context switch
        ok
      end,
      ExitCode =
        receive
          {Port, {exit_status, Code}} ->
            Code
        end,
      {ExitCode, lists:flatten(Sofar)}
  end.