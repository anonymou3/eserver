%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 6月 2022 14:59
%%%-------------------------------------------------------------------
-author("new").

-ifndef(ets_name_hrl).
-define(ets_name_hrl, true).

-define(ets_random_seed, ets_random_seed).% 用来存公共种子的ets
-define(ets_id, ets_id).% 生成ID的ets表
-define(ets_role_online, ets_role_online).
-define(ets_role_public, ets_role_public).
-define(ets_union_abstract, ets_union_abstract).
-define(ets_friend_recommend, ets_friend_recommend).
-define(ets_public_data, ets_public_data). %% 公共数据缓存
-define(ets_role_misc, ets_role_misc). %% 杂项数据缓存

%%please  keep  endif at the tail of the file
-endif.