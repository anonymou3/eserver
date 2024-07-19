% @doc 玩家数据持久化

-module(role_persist).
-include("common.hrl").
-include("def_pflag.hrl").
-include("def_role.hrl").
-include("def_mnesia.hrl").
-compile(export_all).


persist_all_data() ->
  RoleID = role_data:get_roleID(),
  case ?TRY_CATCH_2(persist_all_data(RoleID)) of
    error ->
      persist_error(RoleID);      %%进入最后保护的流程
    _ ->
      db_mnesia:delete_role_persist(RoleID)    %%持久化完美成功,mnesia中任何错误数据都在mysql一次成功后删除
  end.

persist_all_data(RoleID) ->
  lager:info("role_persist persist_all_data, RoleID is ~w", [RoleID]),
  do_interval_persist(true).

%%获取需要周期保存的数据
get_persisit_func() ->
  [
    {check_persist_role_misc, get_persist_role_misc, persist_role_misc}
    , {check_persist_gift_code, get_persist_gift_code, persist_gift_code}
  ].

%% 周期保存的数据
do_interval_persist(Force) ->
  RoleID = role_data:get_roleID(),
  lists:foreach(fun({CheckFun, GetFun, SqlFun}) ->
    case ?MODULE:CheckFun(Force) of
      true ->
        Arg = ?MODULE:GetFun(RoleID),
        ?MODULE:SqlFun(RoleID, Arg);
      false ->
        ignore
    end
  end, get_persisit_func()),
  lager:info("role_persist do_interval_persist, RoleID is ~w", [RoleID]),
  ok.

check_persist_role_misc(Force) ->
  case Force orelse erlang:get(?pflag_role_misc) =:= true of
    true ->
      true;
    false ->
      false
  end.
get_persist_role_misc(_RoleID) ->
  role_data:get_role_misc().
persist_role_misc(_RoleID, RoleMisc) ->
  case RoleMisc of
    #role_misc{} ->
      db_sql:set_role_misc(RoleMisc);
    _ ->
      ignore
  end,
  put(?pflag_role_misc, false).

%%
%%%%pflag_init用于在其他地方dirty他，一定立即下次落地时间到就落地mysql
%%check_persist_roleInit(Force) ->
%%  case Force orelse erlang:get(?pflag_init) of
%%    true ->
%%      true;
%%    _ ->
%%      false
%%  end.
%%get_persist_roleInit(_RoleID) ->
%%  role_data:get_roleInit().
%%persist_roleInit(_RoleID, Arg) ->
%%  db_sql:set_roleInit(Arg),
%%  erlang:put(?pflag_init, false).
%%
%%check_persist_roleInfo(Force) ->
%%  case Force orelse erlang:get(?pflag_role) of
%%    true ->
%%      true;
%%    _ ->
%%      false
%%  end.
%%get_persist_roleInfo(_RoleID) ->
%%  Ger = role_data:get_handbookGer(),
%%  Soul = role_data:get_handbookSoul(),
%%  Equip = role_data:get_handbookEquip(),
%%  RoleInfo = role_data:get_roleInfo(),
%%  RoleVar = role_var:getRoleVar(),
%%  HandBookCoun = length(Ger) + length(Equip) + length(Soul),
%%  RoleInfo2 = RoleInfo#role{lastLogoutTime = util:now(),
%%    handbookNum = HandBookCoun,
%%    vararray = RoleVar
%%  },
%%  RoleTimes = role_data:get_roleTimes(),
%%  [RoleInfo2, RoleTimes].
%%persist_roleInfo(_RoleID, [RoleInfo, RoleTimes]) ->
%%  db_sql:upate_roleInfo(RoleInfo),
%%  role_lib:update_rolePublic(RoleInfo, RoleTimes),
%%  put(?pflag_role, false).
%%
%%这是一段真的危机关头救人的代码，不建议删除
persist_error(RoleID) ->
  RoleData =
    lists:map(fun({_CheckFun, DataFun, _SqlFun}) ->
      {DataFun, ?MODULE:DataFun(RoleID)}
    end, get_persisit_func()),
  db_mnesia:write_role_persist(RoleID, RoleData).

%%数据修复过程
%%{mdb_role_persist,RoleID,RoleData,Time} = db_mnesia:get_role_persist(RoleID).
%%{get_persist_role_title,RoleTitle} = lists:keyfind(get_persist_role_title,1,RoleData).
%%NewRoleTitle = RoleTitle#role_title{sign = []}.
%%NewRoleData = lists:keystore(get_persist_role_title,1,RoleData,{get_persist_role_title,NewRoleTitle}).
%%db_mnesia:write_role_persist(RoleID,NewRoleData)

%%可以尝试恢复玩家数据的代码
try_save_role_error_data(RoleID) ->
  case db_mnesia:get_role_persist(RoleID) of
    #mdb_role_persist{roleData = FuncDataList, time = Time} = MdbRolePersist ->
      case util:now() >= Time + 10 of
        true ->
          FunList = get_persisit_func(),
          lager:error("now doing data back roleID = ~w", [RoleID]),
          Do =
            util:foldl(fun(FuncData, Acc) ->
              case FuncData of
                {DataFun, Data} ->

                  {_CheckFun, _DataFun, SqlFun} = lists:keyfind(DataFun, 2, FunList),
                  case ?TRY_CATCH_2(?MODULE:SqlFun(RoleID, Data)) of
                    ?error ->
                      lager:error("back sql wrong,SqlFun = ~w,RoleID = ~w,Data = ~w", [SqlFun, RoleID, Data]),
                      {return, false};
                    _ ->
                      Acc
                  end;
                _ ->
                  lager:error("Func Data error ~w~n", [FuncData]),
                  throw(func_data_error)
              end
            end, true, FuncDataList),
          case Do of
            true ->
              db_mnesia:delete_role_persist(RoleID),
              true;
            false ->
              MDBName = db_mnesia:get_role_persist_hash(RoleID),
              NewMDBRolePersist = MdbRolePersist#mdb_role_persist{time = util:now()},
              mnesia:dirty_write(MDBName, NewMDBRolePersist),
              mnesia_wirte_db_error            %%mnesia写到mysql出错
          end;
        false ->
          mnesia_write_db_too_frenquency      %%mnesia写到mysql过于频繁
      end;
    _ ->
      true
  end.

check_role_mnesia_persist(RoleID) ->
  case try_save_role_error_data(RoleID) of
    true ->
      true;
    _Err ->
      throw(try_save_role_error_data_wrong)
  end.