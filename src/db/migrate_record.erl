-module(migrate_record).
-include("migrate_list.hrl").
-author("new").

-compile(export_all).

do_migrate(MigrationName) ->
  apply(?MODULE, MigrationName, []),
  SaveSql = io_lib:format("
  insert into g_migration (name, datetime) values(\"~s\",'~s');
  ", [MigrationName, db_sql:datetime(erlang:localtime())]),
  case db_sql:sql_execute_with_log(SaveSql) of
    {ok, _} ->
      ok;
    Error ->
      throw({migration_error, MigrationName, Error})
  end.

%%%%g_fighter_listb表see字段问题,修改示例
%%record_change_g_fighter_list_see_2020_11_9() ->
%%  Sql = "select roleID,see from g_fighter_list",
%%  lists:foreach(
%%    fun([RoleID, SeeBin]) ->
%%      SeeTermList = db_sql:to_term(SeeBin),
%%      NewSeeTermList = lists:map(
%%        fun(SeeTerm) ->
%%          case erlang:tuple_size(SeeTerm) of
%%            15 ->
%%              erlang:append_element(SeeTerm, 0);
%%            _ -> SeeTerm
%%          end
%%        end,
%%        SeeTermList
%%      ),
%%      NewSql = io_lib:format("update g_fighter_list set see = ~s where roleID=~w;", [db_sql:to_bin(NewSeeTermList), RoleID]),
%%      {ok, _} = db_sql:sql_execute_with_log(NewSql)
%%    end,
%%    db_sql:get_all(Sql)
%%  ).
%%
%%record_operations_qian_2020_12_1() ->
%%  Sql = "select roleID,qianOperation from g_role_operations",
%%  lists:foreach(
%%    fun(RoleData) ->
%%      case RoleData of
%%        [RoleID, QianOperationBin] ->
%%          QianOperation = db_sql:to_term(QianOperationBin, role_operations:new_r_operations_qian(0)),
%%          case db_sql:get_roleInfo(RoleID) of
%%            #role{dictData = DictData} = RoleInfo ->
%%              NewDictData = dict:store(?type_dict_operations_qian, QianOperation, DictData),
%%              db_sql:upate_roleInfo(RoleInfo#role{dictData = NewDictData});
%%            _ ->
%%              ignore
%%          end;
%%        _ ->
%%          ignore
%%      end
%%    end,
%%    db_sql:get_all(Sql)
%%  ).
%%
%%record_g_role_headframeIDList_2021_3_11() ->
%%  Sql = "select roleID,headframeIDList from g_role",
%%  lists:foreach(
%%    fun([RoleID, HeadframeIDListBin]) ->
%%      HeadframeIDList = db_sql:to_term(HeadframeIDListBin),
%%      {NewList, _} = lists:foldl(
%%        fun(ID, {Acc1, Acc2}) ->
%%          case lists:member(ID, Acc2) of
%%            false ->
%%              {[#r_portrait{portraitID = ID, endSec = role_headframe:get_headframe_endts(ID)} | Acc1], [ID | Acc2]};
%%            _ ->
%%              {Acc1, Acc2}
%%          end
%%        end,
%%        {[], []},
%%        HeadframeIDList),
%%      NewSql = io_lib:format("update g_role set headframeIDList = ~s where roleID=~w;", [db_sql:to_bin(NewList), RoleID]),
%%      {ok, _} = db_sql:sql_execute_with_log(NewSql)
%%    end,
%%    db_sql:get_all(Sql)
%%  ).
%%
%%record_g_role_title_getTitleList_2021_3_15() ->
%%  Sql = "select roleID,getTitleList from g_role_title",
%%
%%  lists:foreach(
%%    fun([RoleID, TitleListBin]) ->
%%      TitleList = db_sql:to_term(TitleListBin),
%%      NewList = lists:map(
%%        fun(#r_title{titleID = ID}) ->
%%          #r_title{titleID = ID, endSec = role_title:get_title_endts(ID)}
%%        end,
%%        TitleList),
%%      NewSql = io_lib:format("update g_role_title set getTitleList = ~s where roleID=~w;", [db_sql:to_bin(NewList), RoleID]),
%%      {ok, _} = db_sql:sql_execute_with_log(NewSql)
%%    end,
%%    db_sql:get_all(Sql)
%%  ).
%%
%%%%好友数据文件转移到数据库
%%friend_data_transfer_db_2021_8_26() ->
%%  {ResAcc, _} =
%%    lists:foldl(
%%      fun(RoleID, {Acc, Num}) ->
%%        case mnesia:dirty_read(?MDB_FRIEND, RoleID) of
%%          [#mdb_friend{value = Value} | _] ->
%%            case Num >= 500 of
%%              true ->
%%                db_sql:make_sql_batch_by_piece("insert into g_friend values", "(~w,~s)", Acc, 1000),
%%                {[[RoleID, db_sql:to_bin(Value)]], 0};
%%              _ ->
%%                {[[RoleID, db_sql:to_bin(Value)] | Acc], Num + 1}
%%            end;
%%          _ ->
%%            {Acc, Num}
%%        end
%%      end, {[], 0}, mnesia:dirty_all_keys(?MDB_FRIEND)),
%%  case length(ResAcc) > 0 of
%%    true ->
%%      db_sql:make_sql_batch_by_piece("insert into g_friend values", "(~w,~s)", ResAcc, 1000);
%%    _ -> ignore
%%  end.
%%
%%%%宝青阁数据库修复
%%record_g_role_additional_wish_2022_4_16() ->
%%  SQL = "select roleID,wish from g_role_additional;",
%%  case db_sql:get_all_with_err(SQL) of
%%    List when is_list(List) ->
%%      lists:foreach(fun([RoleID, Wish]) ->
%%        Data = db_sql:to_term(Wish),
%%        NWish =
%%          case Data of
%%            {r_wish_activity, IsOpen, ActivityID, Session, Loop, IndexList, Wished, NowWish, Grid, IsOver, WishIndex, IsNew} ->
%%              {r_wish_activity, IsOpen, ActivityID, Session, Loop, IndexList, Wished, NowWish, Grid, IsOver, WishIndex, IsNew, []};
%%            {r_wish_activity, _, _, _, _, _, _, _, _, _, _, _, _} ->
%%              Data
%%          end,
%%        UpdateSql = io_lib:format("update g_role_additional set wish=~s where roleID = ~w;", [db_sql:to_bin(NWish), RoleID]),
%%        db_sql:sql_execute_with_log(UpdateSql)
%%                    end, List);
%%    _ ->
%%      io:format("error record_g_role_additional_wish_2022_4_16!!!"),
%%      throw(record_g_role_additional_wish_2022_4_16)
%%  end.
%%---------------------------------------do_migrate func end-------------------------------------------
