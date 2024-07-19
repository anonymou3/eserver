-module(migrate).
-author("new").

-include("migrate_list.hrl").
-include("common.hrl").

-compile(export_all).

%% external functions
check() ->
  Migrated = get_migrated(),
  lists:foreach(fun({MigrateRoute, MigrationName}) ->
    case lists:member(MigrationName, Migrated) of
      false ->
        MigrateRoute:do_migrate(MigrationName);
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
  case db_sql:sql_execute_with_log("select * from g_migration;") of
    {ok, List} ->
      [util:to_atom(Name) || [Name, _Datetime] <- List];
    {error, {<<"42S02">>, _}} ->
      db_sql:sql_execute_with_log(InitSql),
      [];
    Error ->
      {error, Error}
  end.

clear_except_migration() ->
  Tables = db_sql:get_flat("show tables"),
  Tables0 = lists:delete(<<"g_migration">>, Tables),
  case Tables0 of
    [] ->
      [];
    _ ->
      [["drop table ", Table, $;] || Table <- Tables0]
  end.

do_migrate(Name) when is_atom(Name) ->
  SaveSql = io_lib:format("
  insert into g_migration (name, datetime) values(\"~s\",'~s');
  ", [Name, db_sql:datetime(erlang:localtime())]),
  Sql = util:latin1(?MODULE:Name() ++ SaveSql),
  case db_sql:sql_execute_with_log(Sql) of
    {ok, _} ->
      ok;
    Error ->
      throw({migration_error, Name, Error})
  end.

migrations() ->
  ?migrate_do_list.

create_tables() ->
  "
-- 导出  表 g_migration 结构
CREATE TABLE IF NOT EXISTS `g_migration` (
  `name` varchar(128) NOT NULL,
  `datetime` datetime NOT NULL,
  PRIMARY KEY (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Table structure for t_email
-- ----------------------------
-- DROP TABLE IF EXISTS `t_email`;
CREATE TABLE IF NOT EXISTS `t_email` (
  `id` bigint(20) NOT NULL,
  `idx` int(11) DEFAULT '0',
  `receiveid` bigint(20) NOT NULL DEFAULT '0',
  `sendid` int(11) DEFAULT '0',
  `theme` varchar(64) COLLATE utf8mb4_bin DEFAULT '',
  `sender` varchar(64) COLLATE utf8mb4_bin DEFAULT NULL,
  `content` varchar(1024) COLLATE utf8mb4_bin DEFAULT '',
  `items` varchar(5120) COLLATE utf8mb4_bin DEFAULT NULL,
  `flag` int(11) DEFAULT '0',
  `createtime` int(11) NOT NULL,
  `deletetime` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `receiveid` (`receiveid`) USING HASH,
  KEY `createtime` (`createtime`) USING HASH,
  KEY `deletetime` (`deletetime`) USING HASH,
  KEY `idx` (`idx`) USING HASH
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_emailflag
-- ----------------------------
-- DROP TABLE IF EXISTS `t_emailflag`;
CREATE TABLE IF NOT EXISTS `t_emailflag` (
  `receiveid` bigint(20) NOT NULL DEFAULT '0',
  `idx` int(11) NOT NULL DEFAULT '0',
  `deletetime` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`receiveid`,`idx`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_emailsys
-- ----------------------------
-- DROP TABLE IF EXISTS `t_emailsys`;
CREATE TABLE IF NOT EXISTS `t_emailsys` (
  `idx` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `theme` varchar(64) COLLATE utf8mb4_bin DEFAULT '',
  `sender` varchar(64) COLLATE utf8mb4_bin DEFAULT NULL,
  `content` varchar(1024) COLLATE utf8mb4_bin DEFAULT '',
  `items` varchar(5120) COLLATE utf8mb4_bin DEFAULT '0',
  `createtime` int(11) NOT NULL DEFAULT '0',
  `deletetime` int(11) NOT NULL DEFAULT '0',
  `type` int(11) DEFAULT '0',
  `condition` varchar(1024) COLLATE utf8mb4_bin DEFAULT '',
  PRIMARY KEY (`idx`),
  KEY `deletetime` (`deletetime`) USING HASH
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_friends
-- ----------------------------
-- DROP TABLE IF EXISTS `t_friends`;
CREATE TABLE IF NOT EXISTS `t_friends` (
  `idx` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `owner` bigint(20) NOT NULL DEFAULT '0',
  `ownsid` bigint(20) NOT NULL,
  `friend` bigint(20) NOT NULL DEFAULT '0',
  `friendsid` bigint(20) NOT NULL,
  `flag` int(11) NOT NULL DEFAULT '0',
  `create` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`idx`),
  KEY `owner` (`owner`) USING HASH,
  KEY `friend` (`friend`) USING HASH
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_friends_info
-- ----------------------------
-- DROP TABLE IF EXISTS `t_friends_info`;
CREATE TABLE IF NOT EXISTS `t_friends_info` (
  `rid` bigint(20) NOT NULL DEFAULT '0',
  `rname` varchar(50) COLLATE utf8mb4_bin NOT NULL DEFAULT '',
  `icon` int(11) NOT NULL DEFAULT '0',
  `sex` int(11) NOT NULL DEFAULT '0',
  `vip` int(11) NOT NULL DEFAULT '0',
  `gold` int(11) NOT NULL DEFAULT '0',
  `sign` varchar(50) COLLATE utf8mb4_bin NOT NULL DEFAULT '',
  `statis` blob,
  PRIMARY KEY (`rid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_global
-- ----------------------------
-- DROP TABLE IF EXISTS `t_global`;
CREATE TABLE IF NOT EXISTS `t_global` (
  `id` varchar(50) COLLATE utf8mb4_bin NOT NULL,
  `val` blob,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_misc
-- ----------------------------
-- DROP TABLE IF EXISTS `t_misc`;
CREATE TABLE IF NOT EXISTS `t_misc` (
  `key` int(11) NOT NULL,
  `value` blob NOT NULL,
  PRIMARY KEY (`key`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_bag
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_bag`;
CREATE TABLE IF NOT EXISTS `t_mod_bag` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_baseinfo
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_baseinfo`;
CREATE TABLE IF NOT EXISTS `t_mod_baseinfo` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_body
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_body`;
CREATE TABLE IF NOT EXISTS `t_mod_body` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_charge
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_charge`;
CREATE TABLE IF NOT EXISTS `t_mod_charge` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_chat
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_chat`;
CREATE TABLE IF NOT EXISTS `t_mod_chat` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_draw
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_draw`;
CREATE TABLE IF NOT EXISTS `t_mod_draw` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_dungeon
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_dungeon`;
CREATE TABLE IF NOT EXISTS `t_mod_dungeon` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_equip
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_equip`;
CREATE TABLE IF NOT EXISTS `t_mod_equip` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_extra
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_extra`;
CREATE TABLE IF NOT EXISTS `t_mod_extra` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_fight
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_fight`;
CREATE TABLE IF NOT EXISTS `t_mod_fight` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_forest
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_forest`;
CREATE TABLE IF NOT EXISTS `t_mod_forest` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_friend
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_friend`;
CREATE TABLE IF NOT EXISTS `t_mod_friend` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_ground
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_ground`;
CREATE TABLE IF NOT EXISTS `t_mod_ground` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_hbook
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_hbook`;
CREATE TABLE IF NOT EXISTS `t_mod_hbook` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_hero
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_hero`;
CREATE TABLE IF NOT EXISTS `t_mod_hero` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_maze
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_maze`;
CREATE TABLE IF NOT EXISTS `t_mod_maze` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` bigint(20) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_misc
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_misc`;
CREATE TABLE IF NOT EXISTS `t_mod_misc` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_onwu
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_onwu`;
CREATE TABLE IF NOT EXISTS `t_mod_onwu` (
  `id` bigint(20) NOT NULL,
  `val` mediumblob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_pet
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_pet`;
CREATE TABLE IF NOT EXISTS `t_mod_pet` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_rank
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_rank`;
CREATE TABLE IF NOT EXISTS `t_mod_rank` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_sequip
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_sequip`;
CREATE TABLE IF NOT EXISTS `t_mod_sequip` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_shop
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_shop`;
CREATE TABLE IF NOT EXISTS `t_mod_shop` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_soulpos
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_soulpos`;
CREATE TABLE IF NOT EXISTS `t_mod_soulpos` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_task
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_task`;
CREATE TABLE IF NOT EXISTS `t_mod_task` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_tower
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_tower`;
CREATE TABLE IF NOT EXISTS `t_mod_tower` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mod_trail
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mod_trail`;
CREATE TABLE IF NOT EXISTS `t_mod_trail` (
  `id` bigint(20) NOT NULL,
  `val` blob,
  `ver` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_mysqlauto
-- ----------------------------
-- DROP TABLE IF EXISTS `t_mysqlauto`;
CREATE TABLE IF NOT EXISTS `t_mysqlauto` (
  `name` varchar(50) COLLATE utf8mb4_bin NOT NULL DEFAULT '',
  PRIMARY KEY (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_player
-- ----------------------------
-- DROP TABLE IF EXISTS `t_player`;
CREATE TABLE IF NOT EXISTS `t_player` (
  `rid` bigint(20) NOT NULL,
  `rname` varchar(50) COLLATE utf8mb4_bin NOT NULL,
  `uid` varchar(50) COLLATE utf8mb4_bin NOT NULL,
  `sid` int(11) NOT NULL DEFAULT '0',
  `createtime` int(11) NOT NULL DEFAULT '0',
  `logouttime` int(11) DEFAULT '0',
  `lasttime` int(11) DEFAULT '0',
  `sex` int(11) DEFAULT '0',
  `job` int(11) DEFAULT '0',
  `icon` int(11) DEFAULT '1',
  `level` int(11) DEFAULT '0',
  `exp` int(11) DEFAULT '0',
  `power` int(11) DEFAULT '0' COMMENT '战力',
  `energy` int(11) DEFAULT '0' COMMENT '体力',
  `silver` int(11) DEFAULT '0' COMMENT '银两',
  `gold` int(11) DEFAULT '0' COMMENT '赠送元宝',
  `diamond` int(11) DEFAULT '0' COMMENT '充值元宝',
  `vip` int(11) DEFAULT NULL COMMENT 'vip',
  PRIMARY KEY (`rid`),
  KEY `rname` (`rname`) USING HASH,
  KEY `uid` (`uid`) USING HASH,
  KEY `sid` (`logouttime`) USING HASH
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_rank_cache
-- ----------------------------
-- DROP TABLE IF EXISTS `t_rank_cache`;
CREATE TABLE IF NOT EXISTS `t_rank_cache` (
  `rid` bigint(20) NOT NULL,
  `cache` mediumblob,
  PRIMARY KEY (`rid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_rank_onwu
-- ----------------------------
-- DROP TABLE IF EXISTS `t_rank_onwu`;
CREATE TABLE IF NOT EXISTS `t_rank_onwu` (
  `rank` int(11) NOT NULL,
  `rid` bigint(20) NOT NULL,
  `power` bigint(20) NOT NULL,
  `last` int(11) NOT NULL,
  `cache` mediumblob,
  PRIMARY KEY (`rank`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_rank_tp1
-- ----------------------------
-- DROP TABLE IF EXISTS `t_rank_tp1`;
CREATE TABLE IF NOT EXISTS `t_rank_tp1` (
  `rid` bigint(20) NOT NULL,
  `val` bigint(20) NOT NULL,
  `time` int(20) NOT NULL,
  PRIMARY KEY (`rid`),
  KEY `val` (`val`) USING HASH
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_rank_tp101
-- ----------------------------
-- DROP TABLE IF EXISTS `t_rank_tp101`;
CREATE TABLE IF NOT EXISTS `t_rank_tp101` (
  `rid` bigint(20) NOT NULL,
  `val` bigint(20) NOT NULL,
  `time` int(20) NOT NULL,
  PRIMARY KEY (`rid`),
  KEY `val` (`val`) USING HASH
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_rank_tp102
-- ----------------------------
-- DROP TABLE IF EXISTS `t_rank_tp102`;
CREATE TABLE IF NOT EXISTS `t_rank_tp102` (
  `rid` bigint(20) NOT NULL,
  `val` bigint(20) NOT NULL,
  `time` int(20) NOT NULL,
  PRIMARY KEY (`rid`),
  KEY `val` (`val`) USING HASH
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_rank_tp103
-- ----------------------------
-- DROP TABLE IF EXISTS `t_rank_tp103`;
CREATE TABLE IF NOT EXISTS `t_rank_tp103` (
  `rid` bigint(20) NOT NULL,
  `val` bigint(20) NOT NULL,
  `time` int(20) NOT NULL,
  PRIMARY KEY (`rid`),
  KEY `val` (`val`) USING HASH
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_rank_tp2
-- ----------------------------
-- DROP TABLE IF EXISTS `t_rank_tp2`;
CREATE TABLE IF NOT EXISTS `t_rank_tp2` (
  `rid` bigint(20) NOT NULL,
  `val` bigint(20) NOT NULL,
  `time` int(20) NOT NULL,
  PRIMARY KEY (`rid`),
  KEY `val` (`val`) USING HASH
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_rank_tp3
-- ----------------------------
-- DROP TABLE IF EXISTS `t_rank_tp3`;
CREATE TABLE IF NOT EXISTS `t_rank_tp3` (
  `rid` bigint(20) NOT NULL,
  `val` bigint(20) NOT NULL,
  `time` int(20) NOT NULL,
  PRIMARY KEY (`rid`),
  KEY `val` (`val`) USING HASH
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_rank_tp4
-- ----------------------------
-- DROP TABLE IF EXISTS `t_rank_tp4`;
CREATE TABLE IF NOT EXISTS `t_rank_tp4` (
  `rid` bigint(20) NOT NULL,
  `val` bigint(20) NOT NULL,
  `time` int(20) NOT NULL,
  PRIMARY KEY (`rid`),
  KEY `val` (`val`) USING HASH
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_rank_tp5
-- ----------------------------
-- DROP TABLE IF EXISTS `t_rank_tp5`;
CREATE TABLE IF NOT EXISTS `t_rank_tp5` (
  `rid` bigint(20) NOT NULL,
  `val` bigint(20) NOT NULL,
  `time` int(20) NOT NULL,
  PRIMARY KEY (`rid`),
  KEY `val` (`val`) USING HASH
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_rank_tp6
-- ----------------------------
-- DROP TABLE IF EXISTS `t_rank_tp6`;
CREATE TABLE IF NOT EXISTS `t_rank_tp6` (
  `rid` bigint(20) NOT NULL,
  `val` bigint(20) NOT NULL,
  `time` int(20) NOT NULL,
  PRIMARY KEY (`rid`),
  KEY `val` (`val`) USING HASH
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_rank_tp7
-- ----------------------------
-- DROP TABLE IF EXISTS `t_rank_tp7`;
CREATE TABLE IF NOT EXISTS `t_rank_tp7` (
  `rid` bigint(20) NOT NULL,
  `val` bigint(20) NOT NULL,
  `time` int(20) NOT NULL,
  PRIMARY KEY (`rid`),
  KEY `val` (`val`) USING HASH
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_rank_world
-- ----------------------------
-- DROP TABLE IF EXISTS `t_rank_world`;
CREATE TABLE IF NOT EXISTS `t_rank_world` (
  `id` int(11) NOT NULL,
  `time` int(11) NOT NULL,
  `rid` bigint(20) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- ----------------------------
-- Table structure for t_replay
-- ----------------------------
-- DROP TABLE IF EXISTS `t_replay`;
CREATE TABLE IF NOT EXISTS `t_replay` (
  `k1` bigint(20) NOT NULL,
  `k2` bigint(20) NOT NULL,
  `val` mediumblob,
  UNIQUE KEY `k` (`k1`,`k2`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
".

add_column_g_role_religiousID_2020_11_29() ->
  "
  ALTER TABLE `g_role`
	ADD COLUMN `religiousID` smallint(5) NOT NULL DEFAULT '1' COMMENT '道行等级ID' AFTER `maxTotalScore`,
	ADD COLUMN `headframeID`  INT(11) NOT NULL DEFAULT '1' COMMENT '头像框ID' AFTER `religiousID`,
	ADD COLUMN `headframeIDList` VARBINARY(500) DEFAULT NULL COMMENT '已经获取到的头像框列表' AFTER `headframeID`;
  ".

create_g_role_misc_2023_3_16() ->
  "
  CREATE TABLE IF NOT EXISTS `g_role_misc` (
    `roleID` BIGINT(20) NOT NULL COMMENT '角色ID',
	`dictData` blob NOT NULL COMMENT '杂项数据',
    PRIMARY KEY (`roleID`)
  ) COMMENT='玩家杂项表' ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
  ".