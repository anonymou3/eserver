%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 6月 2022 14:59
%%%-------------------------------------------------------------------
-author("new").

-ifndef(def_mnesia).
-define(def_mnesia, true).

-record(player, {username, password, uuid, logintime, logouttime}).

-define(MDB_ROLE_PERSIST0, mdb_role_persist0).
-define(MDB_ROLE_PERSIST1, mdb_role_persist1).
-define(MDB_ROLE_PERSIST2, mdb_role_persist2).
-define(MDB_ROLE_PERSIST3, mdb_role_persist3).
-define(MDB_ROLE_PERSIST4, mdb_role_persist4).
-define(MDB_ROLE_PERSIST5, mdb_role_persist5).
-define(MDB_ROLE_PERSIST6, mdb_role_persist6).
-define(MDB_ROLE_PERSIST7, mdb_role_persist7).
-define(MDB_ROLE_PERSIST8, mdb_role_persist8).
-define(MDB_ROLE_PERSIST9, mdb_role_persist9).
-record(mdb_role_persist, {
  roleID,                %%玩家ID
  roleData,              %%玩家的持久化出错的数据
  time                  %%玩家持久化出错的时间
}).

%%please  keep  endif at the tail of the file
-endif.