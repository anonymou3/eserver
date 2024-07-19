-module(migrate_log).
-author("new").

-compile(export_all).

-include("common.hrl").
-include("migrate_list.hrl").

%% external functions
check() ->
  Migrated = get_migrated(),
  lists:foreach(fun({_, MigrateKey} = MigrationName) ->
    case lists:member(MigrateKey, Migrated) of
      false ->
        do_migrate(MigrationName);
      _ ->
        ok
    end
                end, migrations()).

check_standalone() ->
  db:connect_standalone(),
  check().

%% internal functions
get_migrated() ->
  InitSql = "
CREATE TABLE IF NOT EXISTS `g_migration` (
`name` VARCHAR(128) NOT NULL,
`datetime` DATETIME NOT NULL,
PRIMARY KEY (`name`)
)
ENGINE=InnoDB;
	",
  case db_sql:sql_execute_with_log(?db_log, "select * from g_migration;") of
    {ok, List} ->
      [util:to_atom(Name) || [Name, _Datetime] <- List];
    {error, {<<"42S02">>, _}} ->
      db_sql:sql_execute_with_log(?db_log, InitSql),
      [];
    Error ->
      {error, Error}
  end.

clear_except_migration() ->
  Tables = db_sql:get_flat(?db_log, "show tables"),
  Tables0 = lists:delete(<<"g_migration">>, Tables),
  case Tables0 of
    [] ->
      [];
    _ ->
      [["drop table ", Table, $;] || Table <- Tables0]
  end.

do_migrate({?log_migrate, Name}) when is_atom(Name) ->
  SaveSql = io_lib:format("
  insert into g_migration (name, datetime) values(\"~s\",'~s');
  ", [Name, db_sql:datetime(erlang:localtime())]),
  Sql = util:latin1(?MODULE:Name() ++ SaveSql),
  case db_sql:sql_execute_with_log(?db_log, Sql) of
    {ok, _} ->
      ok;
    Error ->
      throw({migration_error, Name, Error})
  end;
do_migrate({?log_migrate_record, Name}) when is_atom(Name) ->
  true = ?MODULE:Name(),
  SaveSql = io_lib:format("
  insert into g_migration (name, datetime) values(\"~s\",'~s');
  ", [Name, db_sql:datetime(erlang:localtime())]),
  Sql = util:latin1(SaveSql),
  case db_sql:sql_execute_with_log(?db_log, Sql) of
    {ok, _} ->
      ok;
    Error ->
      throw({migration_error, Name, Error})
  end.


migrations() ->
  [
    {?log_migrate, clear_log_2020_11_26}
    , {?log_migrate, rebuild_lz_game_log_2020_11_26}
  ].

clear_log_2020_11_26() ->
  clear_except_migration().

rebuild_lz_game_log_2020_11_26() ->
  "
-- 导出  表 u3d_gameserver_log.g_migration 结构
CREATE TABLE IF NOT EXISTS `g_migration` (
  `name` varchar(128) NOT NULL,
  `datetime` datetime NOT NULL,
  PRIMARY KEY (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- 数据导出被取消选择。

  ".
%%
%%create_role_ask_sword_log_2021_3_2() ->
%%  "
%%  CREATE TABLE IF NOT EXISTS `t_role_ask_sword_log` (
%%  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '自增主键',
%%  `roleID` bigint(20) DEFAULT NULL COMMENT '角色ID',
%%  `season` int(11) unsigned NOT NULL COMMENT '赛季',
%%  `win` smallint(6) unsigned NOT NULL COMMENT '输赢',
%%  `level` smallint(6) unsigned NOT NULL COMMENT '段位',
%%  `scores` int(11) unsigned NOT NULL COMMENT '积分',
%%  `money` int(11) unsigned NOT NULL COMMENT '问剑币',
%%  `localRank` int(11) unsigned NOT NULL COMMENT '本服排名',
%%  `worldRank` int(11) unsigned NOT NULL COMMENT '全服排名',
%%  `time` datetime NOT NULL COMMENT '详细时间',
%%  PRIMARY KEY (`id`),
%%  KEY `roleID` (`roleID`)
%%) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='天梯日志记录';
%%  ".
%%
%%change_role_ask_sword_log_2021_3_18() ->
%%  Tables = db_sql:get_flat(?db_log, "show tables"),
%%  Sql = util:latin1("
%%    ALTER TABLE `~s`
%%    ADD COLUMN `teampower` text NOT NULL COMMENT '队伍战力' AFTER `worldRank`;
%%	"),
%%  lists:foreach(fun(TableB) ->
%%    Table = util:to_string(TableB),
%%    case string:str(Table, "t_role_ask_sword_log") > 0 of
%%      true ->
%%        ChangeSql = io_lib:format(Sql, [Table]),
%%        db_sql:sql_execute_with_log(?db_log, ChangeSql);
%%      false ->
%%        ignore
%%    end
%%                end, Tables),
%%  true.
%%
%%alter_t_activity_join_2021_4_28() ->
%%  Tables = db_sql:get_flat(?db_log, "show tables"),
%%  Sql = util:latin1("
%%    ALTER TABLE `~s`
%%    MODIFY COLUMN `argNum`  bigint(20) UNSIGNED NOT NULL COMMENT '参与次数字段' AFTER `activityID`;
%%	"),
%%  lists:foreach(fun(TableB) ->
%%    Table = util:to_string(TableB),
%%    case string:str(Table, "t_activity_join") > 0 of
%%      true ->
%%        ChangeSql = io_lib:format(Sql, [Table]),
%%        db_sql:sql_execute_with_log(?db_log, ChangeSql);
%%      false ->
%%        ignore
%%    end
%%                end, Tables),
%%  true.