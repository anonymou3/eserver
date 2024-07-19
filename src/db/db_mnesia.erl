%% 一些不太重要的数据，又不需要后台查看的可以使用mnesia数据库
%% 使用mnesia保存的数据需保证，即使数据丢失也不影响游戏

-module(db_mnesia).

-include("common.hrl").
-include("def_mnesia.hrl").

-export([start/0]).

-define(DEF_RAM_TABLE(Type, Rec),
  [{ram_copies, [node()]},
    {type, Type},
    {record_name, Rec},
    {attributes, record_info(fields, Rec)}
  ]).

-define(DEF_DISK_TABLE(Type, Rec),
  [{disc_copies, [node()]},
    {type, Type},
    {record_name, Rec},
    {attributes, record_info(fields, Rec)}
  ]).

-define(DEF_DISK_ONLY_TABLE(Type, Rec),
  [{disc_only_copies, [node()]},
    {type, Type},
    {record_name, Rec},
    {attributes, record_info(fields, Rec)}
  ]).

db_init() ->
  case mnesia:system_info(use_dir) of
    true ->
      init_table();
    _ ->
      lager:info("create mnesia database ~p", [node()]),
      mnesia:create_schema([node()])
  end.

init_table() ->
  lists:foreach(
    fun({Tab, Definition}) ->
      case lists:member(Tab, mnesia:system_info(tables)) of
        true ->
          ignore;
        _ ->
          A = mnesia:create_table(Tab, Definition),
          lager:info("create table ~w : ~w", [Tab, A])
      end
    end,
    table_defines()).

start() ->
  mnesia:start(),
  mnesia:change_table_copy_type(schema, node(), disc_copies),
  db_init(),
  mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).

get_role_persist_hash(RoleID) ->
  Hash = RoleID rem 10,
  case Hash of
    0 ->
      ?MDB_ROLE_PERSIST0;
    1 ->
      ?MDB_ROLE_PERSIST1;
    2 ->
      ?MDB_ROLE_PERSIST2;
    3 ->
      ?MDB_ROLE_PERSIST3;
    4 ->
      ?MDB_ROLE_PERSIST4;
    5 ->
      ?MDB_ROLE_PERSIST5;
    6 ->
      ?MDB_ROLE_PERSIST6;
    7 ->
      ?MDB_ROLE_PERSIST7;
    8 ->
      ?MDB_ROLE_PERSIST8;
    9 ->
      ?MDB_ROLE_PERSIST9;
    _ ->
      ?MDB_ROLE_PERSIST0
  end.

%%获取玩家持久化出错的数据
get_role_persist(RoleID) ->
  MDBName = get_role_persist_hash(RoleID),
  case mnesia:dirty_read(MDBName, RoleID) of
    [#mdb_role_persist{} = MDBRolePersist | _] ->
      MDBRolePersist;
    _ ->
      ?undefined
  end.

write_role_persist(RoleID, RoleData) ->
  MDBName = get_role_persist_hash(RoleID),
  MDBRolePersist = #mdb_role_persist{roleID = RoleID, roleData = RoleData, time = 0},
  mnesia:dirty_write(MDBName, MDBRolePersist).

%%删除玩家出错的持久化数据
delete_role_persist(RoleID) ->
  MDBName = get_role_persist_hash(RoleID),
  mnesia:dirty_delete(MDBName, RoleID).

table_defines() ->
  [
    {player, ?DEF_DISK_ONLY_TABLE(bag, player)}
  ].