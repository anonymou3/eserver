%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 6月 2022 19:49
%%%-------------------------------------------------------------------
-module(db_ets).
-author("new").
-include("ets_name.hrl").

%% API
-export([init/0]).

-record(r_friend_recommend_pool, {
  key
}).

init() ->
  ets:new(?ets_id, [{keypos, 1}, set, public, named_table]),
  tk_id:init(),
  ets:new(?ets_random_seed, [{keypos, 1}, named_table, set, public]),
  ets:insert(?ets_random_seed, {seed, rand:uniform(100)}),
  ets:new(?ets_role_online, [{keypos, 1}, set, public, named_table]),
  %% 缓存玩家的role信息,keypos=2,如何清理缓存?
  ets:new(?ets_role_public, [{keypos, 2}, set, public, named_table]),
  %%ets_union_abstract 公会基础信息 {公会ID 名字,等级,公会ID列表}
  ets:new(?ets_union_abstract, [public, named_table, {keypos, 1}, {write_concurrency, true}, {read_concurrency, true}]),
  %%好友推荐池
  ets:new(?ets_friend_recommend, [{keypos, #r_friend_recommend_pool.key}, named_table, ordered_set, public]),
  %%master公共数据normal服的同步缓存
  ets:new(?ets_public_data, [{keypos, 1}, named_table, set, public]),
  todo.