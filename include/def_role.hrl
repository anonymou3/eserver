%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 3月 2023 10:43
%%%-------------------------------------------------------------------
-author("new").

-ifndef(def_role_h).
-define(def_role_h, 1).

-include("common.hrl").
-include("record.hrl").
-include("def_mnesia.hrl").

-define(role_misc, role_misc).     %%玩家杂项

-define(type_misc_newdungeon, 1).      %%四种爬塔
-define(type_misc_newdungeon41_reward, 2).      %%爬塔奖励记录
-define(type_misc_newdungeon42_reward, 3).      %%爬塔奖励记录
-define(type_misc_newdungeon43_reward, 4).      %%爬塔奖励记录
-define(type_misc_newdungeon44_reward, 5).      %%爬塔奖励记录

-endif.

