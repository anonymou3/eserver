%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 6月 2022 11:55
%%%-------------------------------------------------------------------
-author("new").

-ifndef(common_hrl).
-define(common_hrl, 1).

-include("process_name.hrl").
-include("ets_name.hrl").

%%一些基本的工具方法

-define(error, error).
-define(undefined, undefined).
-define(route, route).
-define(self_route, self_route).          %%内部路由用的结构
-define(module_route, module_route).      %%模块路由
-define(return, return).
-define(push(Msg), role_lib:push(Msg)).
-define(db, db).
-define(db_log, db_log).

-define(util_call_timeout, 3000).
-define(util_call_miss, util_call_miss).

-define(integer_min_value, -2147483648).
-define(integer_max_value, 2147483647).
-define(short_max_value, 32767).
-define(rate_max, 10000).  %%比率

-define(one_day_seconds, 86400).
-define(one_week_seconds, 604800).

-define(role_max_num, 1000000).
-define(ROLE_ID_BASE, ((data_setting:get(server_id)) * ?role_max_num)).
-define(FAMILY_ID_BASE, ((data_setting:get(server_id)) * 1000000)).
-define(GER_ID_BASE, ((data_setting:get(server_id)) * (10000 * 10000 * 10000))).
-define(ITEM_ID_BASE, ((data_setting:get(server_id)) * (10000 * 10000 * 10000))).
-define(MAIL_ID_BASE, ((data_setting:get(server_id)) * (10000 * 10000 * 10000))).
-define(REPLAY_ID_BASE, ((data_setting:get(server_id)) * (10000 * 10000 * 10000))).
-define(GERSTAR_ID_BASE, ((data_setting:get(server_id)) * (10000 * 10000 * 10000))).
-define(UNION_AUCTION_BASE, ((data_setting:get(server_id)) * (10000 * 10000 * 10000))).
-define(AUCTION_BASE, ((data_setting:get(server_id)) * (10000 * 10000 * 10000))).
-define(BUFF_BASE, ((data_setting:get(server_id)) * (10000 * 10000 * 10000))).
-define(TEAM_ID_BASE, ((data_setting:get(server_id)) * (10000 * 10000 * 10000))).
-define(SOUND_ID_BASE, ((data_setting:get(server_id)) * (10000 * 10000 * 10000))).
-define(TALK_ID_BASE, ((data_setting:get(server_id)) * (10000 * 10000 * 10000))).
-define(PAY_ORDER_ID_BASE, ((data_setting:get(server_id)) * (10000 * 10000 * 10000))).

-define(CATCH(Expression), (
    try Expression
    catch
      ErrType:Reason:Stack ->
        lager:error("ErrType:~1000p, ErrReason:~1000p, Stack=~100000p, Expression=~s", [ErrType, Reason, Stack, ??Expression]),
        {'EXIT', {ErrType, Reason}}
    end
)).

-define(LOOSE_CATCH(Expression), (
    try Expression
    catch
      throw:SomeThing ->
        SomeThing;
      ErrType:Reason:Stack ->
        lager:error("ErrType:~1000p, ErrReason:~1000p, Stack=~100000p, Expression=~s", [ErrType, Reason, Stack, ??Expression]),
        {'EXIT', {ErrType, Reason}}
    end
)).

-define(TRY_CATCH(Fun, Tip, ErrType, ErrReason),
  try
    Fun
  catch
    ErrType:ErrReason:Stack ->
      lager:error("~ts: ErrType=~w,roleID=~w,Reason=~w,Stacktrace=~w", [Tip, ErrType, erlang:get(?roleID), ErrReason, Stack])
  end).
-define(TRY_CATCH(Fun, ErrType, ErrReason),
  try
    Fun
  catch
    ErrType:ErrReason:Stack ->
      lager:error("ErrType=~w,roleID=~w,Reason=~w,Stacktrace=~w", [ErrType, erlang:get(?roleID), ErrReason, Stack])
  end).
-define(TRY_CATCH(Fun, ErrReason),
  try
    Fun
  catch
    _:ErrReason:Stack ->
      lager:error("Reason=~w,roleID=~w,Stacktrace=~w", [ErrReason, erlang:get(?roleID), Stack])
  end).
-define(TRY_CATCH(Fun), ?TRY_CATCH(Fun, ErrType, ErrReason)).

-define(TRY_CATCH_2(Fun, ErrType, ErrReason),
  try
    Fun
  catch
    ErrType:ErrReason:Stack ->
      lager:error("ErrType=~w,roleID=~w,Reason=~w,Stacktrace=~w", [ErrType, erlang:get(?roleID), ErrReason, Stack]),
      ?error
  end).

-define(TRY_CATCH_2(Fun), ?TRY_CATCH_2(Fun, ErrType, ErrReason)).

-define(ERR_WITH_STACK(Info), (
    try throw(Info)
    catch
      _ErrType:_Reason:Stack ->
        lager:error("INFO is :~s, \n\tStack=~100000p", [Info, Stack]),
        ok
    end
)).

-define(ERR_WITH_STACK(Info, Args), (
    try throw(Info)
    catch
      _ErrType:_Reason:Stack ->
        lager:error("INFO is :~s, \n\tStack=~100000p", [io_lib:format(Info, Args), Stack]),
        ok
    end
)).

%%master公共存储类型 从1开始
-define(master_public_month_fund, 1).
-define(master_public_list, [
  ?master_public_month_fund  %%月基金
]).

%%normal服公共存储类型 从10000开始
-define(normal_power_target_draw_limit, 10000).
-define(normal_public_list, [
  ?normal_power_target_draw_limit %%战力目标奖励本服领取名额限制
]).

%%please  keep  endif at the tail of the file 
-endif.
