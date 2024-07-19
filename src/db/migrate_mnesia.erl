-module(migrate_mnesia).

-include("common.hrl").

-compile(export_all).

%% external functions
check() ->
  Migrated = migrate:get_migrated(),
  lists:foreach(fun(MigrationName) ->
    case lists:member(MigrationName, Migrated) of
      false ->
        do_migrate(MigrationName);
      _ ->
        ok
    end
                end, migrations()).

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

migrations() ->
  [
    record_mdb_r_mail_2020_7_25,
    record_mdb_r_mail_2020_7_28
  ].


%%结构发生了变化的更新
%%update_auto_fight_set_2017_10_10() ->
%%	TransFunc =
%%		fun({mdb_battle_auto_set,RoleID, _})->
%%				{mdb_battle_auto_set,RoleID, 0, 0, []};
%%		   (R) ->
%%				R
%%		end,
%%	Fields = [  roleID, petUID, petSkillID, setList],
%%	{atomic, ok} = mnesia:transform_table(mdb_battle_auto_set, TransFunc, Fields, mdb_battle_auto_set),
%%	ok.

%%数据发生了变化的更新
%% update_pay_log_2() ->
%%	KeyList=mnesia:dirty_all_keys(db_pay_log_p),
%% 		lists:foreach(
%% 	  		fun(RoleID)->
%% 					[R]=mnesia:dirty_read(db_pay_log_p,RoleID),
%% 					mnesia:dirty_write(db_pay_log_p,R#r_pay_log{pay_gold=100})
%% 			end
%% 	  ,KeyList).
%%
%%record_mdb_r_mail_2020_7_25()->
%%		Now = util:now(),
%%		TransFunc =
%%		fun({r_mail,MailUID, RecvID,MailType,SenderID,SenderName,Title,Content,Time,MailTemplateID,ParamList,MailReward,IsRead,IsChange})->
%%				RewardIsGot =
%%					case MailReward of
%%							[]->1;
%%							_->0
%%						end,
%%				{r_mail,MailUID, RecvID,MailType,SenderID,SenderName,Title,Content,Time,MailTemplateID,ParamList,MailReward,IsRead,IsChange,Now + ?MAIL_DEFAULT_EXPIRE_TIME,RewardIsGot,Now + ?MAIL_DEFAULT_EXPIRE_TIME + ?MAIL_DEFAULT_DELETE_TIME};
%%		   (R) ->
%%				R
%%		end,
%%	Fields = [mailUID, recvID,mailType,senderID,senderName,title,content,time,mailTemplateID,paramList,mailReward,isRead,isChange,expireTime,rewardIsGot,autoDelTime],
%%	[{atomic, ok} = mnesia:transform_table(Tab, TransFunc, Fields, r_mail)||Tab<-db_mnesia:mdb_mail_0_9()],
%%	ok.
%%
%%record_mdb_r_mail_2020_7_28()->
%%	Now = util:now(),
%%	TransFunc =
%%		fun({r_mail,MailUID, RecvID,MailType,SenderID,SenderName,Title,Content,Time,MailTemplateID,ParamList,MailReward,IsRead,IsChange,ExpireTime,RewardIsGot})->
%%			{r_mail,MailUID, RecvID,MailType,SenderID,SenderName,Title,Content,Time,MailTemplateID,ParamList,MailReward,IsRead,IsChange,ExpireTime,RewardIsGot,Now + ?MAIL_DEFAULT_EXPIRE_TIME + ?MAIL_DEFAULT_DELETE_TIME};
%%			(R) ->
%%				R
%%		end,
%%	Fields = [mailUID, recvID,mailType,senderID,senderName,title,content,time,mailTemplateID,paramList,mailReward,isRead,isChange,expireTime,rewardIsGot,autoDelTime],
%%	[{atomic, ok} = mnesia:transform_table(Tab, TransFunc, Fields, r_mail)||Tab<-db_mnesia:mdb_mail_0_9()],
%%	ok.
