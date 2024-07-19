%% @doc timer_wheel which can be used in gen_server
-module(timer_wheel).

-compile([{include_size, 30}]).

-include("common.hrl").
-export([
  init/0
  , plan/2
  , add_plan/2
  , work/1
  , get_plan/1
  , get_plan/0
  , clear_plan/1
  , nowsec/0
  , cancel_plan/1
  , tarSec/1
]).

-define(timer_wheel_tick, timer_wheel_tick).
-define(timer_wheel_plan, timer_wheel_plan).

%% 初始化时间轮
init() ->
  NowSec = util:now(),
  put(now, NowSec),
  tick(NowSec).

nowsec() ->
  case erlang:get(now) of
    A when is_integer(A) ->
      A;
    _ ->
      util:now()
  end.

%% 时间轮心跳
tick(Sec) ->
  erlang:send_after(1000, self(), {?timer_wheel_tick, Sec}).

tarSec({TarSec, _Ref}) ->
  TarSec;
tarSec(TarSec) when is_integer(TarSec) ->
  TarSec.


%% 定时器,
%% Fun(TarSec)
%% return: timerKey = {TarSec, Ref} | true
plan(TarSec, Fun) when is_function(Fun, 0) orelse is_function(Fun, 1) ->
  NowSec = util:now(),
  if TarSec =< NowSec ->
    work3(func, Fun, TarSec);
    true ->
      add_plan(TarSec, Fun)
  end;
plan(TarSec, MFA) when is_tuple(MFA) ->
  NowSec = util:now(),
  if TarSec =< NowSec ->
    work3(mfa, MFA, TarSec);
    true ->
      add_plan(TarSec, MFA)
  end.

%% 增加一个计划
%% return: timerKey = {TarSec, Ref}
add_plan(Sec, Fun) when is_function(Fun, 0) orelse is_function(Fun, 1) ->
  Ref = erlang:make_ref(),
  set_plan(Sec, [{Ref, func, Fun} | get_plan(Sec)]),
  {Sec, Ref};
add_plan(Sec, MFA) when is_tuple(MFA) ->
  Ref = erlang:make_ref(),
  set_plan(Sec, [{Ref, mfa, MFA} | get_plan(Sec)]),
  {Sec, Ref}.

%% 设置一秒的安排的所有计划
set_plan(Sec, Plans) ->
  put({?timer_wheel_plan, Sec}, Plans).

%% 获取所有计划
get_plan() ->
  [{Sec, FunList} || {{?timer_wheel_plan, Sec}, FunList} <- erlang:get()].

%% 获取计划
get_plan(Sec) ->
  case erlang:get({?timer_wheel_plan, Sec}) of
    A when is_list(A) ->
      A;
    _ ->
      []
  end.

%% 清空某一秒安排的所有计划，并返回旧的计划
clear_plan(Sec) ->
  case erlang:erase({?timer_wheel_plan, Sec}) of
    A when is_list(A) ->
      A;
    _ ->
      []
  end.

%% 删除计划
cancel_plan(true) ->
  ignore;
cancel_plan({Sec, Ref}) ->
  Plans = get_plan(Sec),
  Plans2 = lists:keydelete(Ref, 1, Plans),
  set_plan(Sec, Plans2).

%% 工作
work(LastTick) when is_integer(LastTick) ->
  NowSec = util:now(),
  put(now, NowSec),
  if LastTick >= NowSec ->
    tick(NowSec);
    true ->
      work(LastTick + 1, NowSec)
  end;
work({Sec, _Ref}) ->
  work2(Sec).

work(NowSec, NowSec) ->
  work2(NowSec),
  tick(NowSec);
work(LastTick, NowSec) ->%要循环一个时间段是因为不能保证一定是每隔一秒会tick一次
  work2(LastTick),
  work(LastTick + 1, NowSec).

work2(Sec) ->
  Plans = clear_plan(Sec),
  lists:foreach(fun({_Ref, Type, F}) ->
    work3(Type, F, Sec)
                end, Plans).

work3(func, F, Sec) when is_function(F, 1) ->
  ?LOOSE_CATCH(F(Sec));
work3(func, F, _Sec) when is_function(F, 0) ->
  ?LOOSE_CATCH(F());
work3(mfa, {M, F, A}, _Sec) ->
  ?LOOSE_CATCH(erlang:apply(M, F, A)).
