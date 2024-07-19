-module(db_sql).
-compile(export_all).
-include("record.hrl").
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-define(log_table_time, log_table_time).
-define(log_table_next, log_table_next).

%%创建数据库
create_databases() ->
  lists:foreach(fun({GameIP, GamePort, GameUser, Password, Database, _GameConnectNum}) ->
    {ok, Pid} = mysql:start_link([{host, GameIP}, {port, GamePort}, {user, GameUser}, {password, Password}, {database, undefined}]),
    Sql = "CREATE DATABASE `" ++ Database ++ "` /*!40100 DEFAULT CHARACTER SET utf8 */",
    case mysql:query(Pid, list_to_binary(Sql)) of
      ok ->
        ok;
      {ok, _ColumnNames, _Rows} ->
        ok;
      {error, {_ErrInt, _ErrCode, Reason}} ->
        lager:info("database create ~p ~p", [Database, Reason])
    end,
    mysql:stop(Pid)
  end, [data_setting:get(?db), data_setting:get(?db_log)]).

% 下面是工具方法========================================================================
sql_execute_with_log(Sql) ->
  sql_execute_with_log(?db, Sql).

%% \mysql\src\mysql_conn.erl keywords:Query and execute calls:
sql_execute_with_log(DataBase, Sql) ->
  case mysql_poolboy:query(DataBase, Sql) of
    ok ->
      {ok, 0};
    {ok, _ColumnNames, Rows} ->
      {ok, Rows};
    {ok, ColumnNamesRowsList} -> %% [{ColumnNames, Rows},...]
      {ok, ColumnNamesRowsList};
    {error, {_ErrInt, ErrCode, Msg}} ->
      lager:error("sql **** ~s ***** execute with err:~p,~s", [Sql, ErrCode, Msg]),
      {error, {ErrCode, Msg}};
    {error, Reason} ->
      lager:error("sql **** ~s ***** execute with err:~p~n", [Sql, Reason]),
      {error, Reason};
    Exception ->
      %%      这两种异常会被抛出
      %%      DDL statements (e.g. CREATE TABLE, ALTER TABLE, etc.) result in %% an implicit commit.
      %%      {implicit_commit, _NestingLevel, Query}
      %%      {implicit_rollback, _NestingLevel, _ServerReason}
      lager:error("sql **** ~s ***** execute with err:~p~n", [Sql, Exception]),
      Exception
  end.

%% 传入的Sql变量为多条sql语句的集合,最好加入事务的保护,保证整个数据操作的原子性
sql_execute_with_log2(Sql) ->
  sql_execute_with_log2(?db, Sql).

%% \mysql\src\mysql.erl keywords:-spec transaction(Conn, TransactionFun, Args, Retries)
sql_execute_with_log2(DataBase, Sql) ->
  case mysql_poolboy:transaction(DataBase, fun(Pid) -> mysql:query(Pid, Sql) end) of
    {atomic, Result} ->
      case Result of
        ok ->
          {ok, 0};
        {ok, _ColumnNames, Rows} ->
          {ok, Rows};
        {ok, ColumnNamesRowsList} -> %% [{ColumnNames, Rows},...]
          {ok, ColumnNamesRowsList};
        Exception ->
          lager:error("sql **** ~s ***** execute with err:~p~n", [Sql, Exception]),
          Exception
      end;
    {aborted, Reason} ->
      lager:error("sql **** ~s ***** execute with err:~p", [Sql, Reason]);
    Exception ->
      lager:error("sql **** ~s ***** execute with err:~p", [Sql, Exception])
  end.

sql_execute_sqls(Sql) ->
  sql_execute_sqls(?db, Sql).

sql_execute_sqls(DataBase, Sql) ->
  case mysql_poolboy:query(DataBase, Sql) of
    ok ->
      {ok, 0};
    {ok, _ColumnNames, Rows} ->
      {ok, Rows};
    {ok, ColumnNamesRowsList} -> %% [{ColumnNames, Rows},...]
      {ok, ColumnNamesRowsList};
    {error, {_ErrInt, ErrCode, Reason2}} ->
      lager:error("sql **** ~s ***** execute with err:~p,~s", [Sql, ErrCode, Reason2]),
      {error, {ErrCode, Reason2}};
    {error, Reason} ->
      lager:error("sql **** ~s ***** execute with err:~p~n", [Sql, Reason]),
      {error, Reason};
    Exception ->
      lager:error("sql **** ~s ***** execute with err:~p~n", [Sql, Exception]),
      Exception
  end.

sql_execute_with_log_state(Statement, Args) ->
  sql_execute_with_log_state(?db, Statement, Args).

sql_execute_with_log_state(DataBase, Statement, Args) ->
  case mysql_poolboy:query(DataBase, Statement, Args) of
    ok ->
      {ok, 0};
    {ok, _ColumnNames, Rows} ->
      {ok, Rows};
    {ok, ColumnNamesRowsList} -> %% [{ColumnNames, Rows},...]
      {ok, ColumnNamesRowsList};
    {error, {_ErrInt, ErrCode, Reason2}} ->
      lager:error("sql ****~p,~p*****execute with err:~p,~s", [Statement, Args, ErrCode, Reason2]),
      {error, {ErrCode, Reason2}};
    {error, Reason} ->
      lager:error("sql ****~p,~p*****execute with err:~p~n", [Statement, Args, Reason]),
      {error, Reason};
    Exception ->
      lager:error("sql ****~p,~p*****execute with err:~p~n", [Statement, Args, Exception]),
      Exception
  end.

get_flat(Sql) ->
  get_flat(?db, Sql).

get_flat(DataBase, Sql) ->
  case sql_execute_with_log(DataBase, Sql) of
    {ok, List} ->
      lists:flatten(List);
    _ ->
      []
  end.

get_all(Sql) ->
  case sql_execute_with_log(?db, Sql) of
    {ok, List} ->
      List;
    _ ->
      []
  end.

get_all(Statement, Sql) ->
  case sql_execute_with_log(Statement, Sql) of
    {ok, List} ->
      List;
    _ ->
      []
  end.

get_all_with_err(Sql) ->
  case sql_execute_with_log(?db, Sql) of
    {ok, List} ->
      List;
    Err ->
      Err
  end.

get_row(Sql) ->
  case sql_execute_with_log(Sql) of
    {ok, List} ->
      case length(List) of
        1 ->
          [List2] = List,
          List2;
        0 ->
          [];
        Num when Num > 1 ->
          [List2 | _] = List,
          List2;
        _ ->
          []
      end;
    _ ->
      []
  end.

get_row(Statement, Sql) ->
  case sql_execute_with_log(Statement, Sql) of
    {ok, List} ->
      case length(List) of
        1 ->
          [List2] = List,
          List2;
        0 ->
          [];
        Num when Num > 1 ->
          [List2 | _] = List,
          List2;
        _ ->
          []
      end;
    _ ->
      []
  end.

%%clean_aleader_info() ->
%%  Sql = io_lib:format("update g_aleader set isSign = 0", []),
%%  sql_execute_with_log(Sql).
%%
%%get_aleader_replay(RUID) ->
%%  Sql = io_lib:format("select replay from g_aleader_replay where replayUID = ~w", [RUID]),
%%  case db_sql:get_row(Sql) of
%%    [Bin] -> to_term(Bin, []);
%%    _ -> ?undefined
%%  end.
%%
%%load_leaders_winner_history(Type) ->
%%  Sql = io_lib:format("select * from g_leaders_winners where type = ~w", [Type]),
%%  case db_sql:get_all(Sql) of
%%    [_ | _] = List ->
%%      Values =
%%        [{Session, [{Winner, S1, 0}, {Seconder, S2, 1}, {Forth1, S3, 2}, {Forth2, S4, 2}, {Eighth1, S5, 3}, {Eighth2, S6, 3}, {Eighth3, S7, 3}, {Eighth4, S8, 3}]}
%%          || [Session, S1, Winner, S2, Seconder, S3, Forth1, S4, Forth2, S5, Eighth1, S6, Eighth2, S7, Eighth3, S8, Eighth4, _Type] <- List],
%%      ets:insert(?ETS_LEADERS_STATUS, {{winners_history, Type}, Values});
%%    _ ->
%%      ignore
%%  end.
%%
%%insert_g_role_talk_setting([[_, _, _, _, _, _] | _] = InsertDataList) ->
%%  RoleIDs = [integer_to_list(RoleID) || [RoleID, _, _, _, _, _] <- InsertDataList],
%%  RoleIDsStr = lists:concat(["(",string:join(RoleIDs,","),")"]),
%%  DeleteSql = io_lib:format("delete from g_role_talk_setting where roleID in ~s", [RoleIDsStr]),
%%  InsertSql = make_sql_batch("insert into g_role_talk_setting values", "(~w,~w,'~s',~w,~w,'~s')", InsertDataList),
%%  sql_execute_sqls(DeleteSql ++ ";" ++ InsertSql).

get_role_misc(RoleID) ->
  Sql = io_lib:format("select dictData from g_role_misc where roleID = ~w;", [RoleID]),
  case get_row(Sql) of
    [DictData] ->
      #role_misc{roleID = RoleID, dictData = to_term(DictData, dict:new())};
    _ ->
      #role_misc{roleID = RoleID, dictData = dict:new()}
  end.

set_role_misc(#role_misc{roleID = RoleID, dictData = DictData}) ->
  Sql = io_lib:format("replace into g_role_misc (roleID, dictData) values (~w,~s);", [RoleID, to_bin(DictData)]),
  {ok, _} = sql_execute_with_log(Sql).


%% ====================================================================
%%日志记录
%% 批量插入
make_sql_batch(Sql, Format, List) when List =/= [] ->
  Str = lists:foldl(fun(E, Acc) ->
    "," ++ io_lib:format(Format, E) ++ Acc
  end, ";", List),
  Sql ++ tl(Str).

%% 分段批量插入
make_sql_batch_by_piece(Sql, Format, List, PieceNum) ->
  make_sql_batch_by_piece_base(?db, Sql, Format, List, PieceNum, 0, "").

make_sql_log_batch_by_piece(Sql, Format, List, PieceNum) ->
  make_sql_batch_by_piece_base(?db, Sql, Format, List, PieceNum, 0, "").

make_sql_batch_by_piece_base(DataBase, Sql, _Format, [], _PieceNum, _AccNum, Acc) ->
  if Acc == "" ->
    ignore;
    true ->
      Sql2 = Sql ++ tl(Acc),
      sql_execute_with_log(DataBase, Sql2)
  end;
make_sql_batch_by_piece_base(DataBase, Sql, Format, List, PieceNum, PieceNum, Acc) ->
  Sql2 = Sql ++ tl(Acc),
  sql_execute_with_log(DataBase, Sql2),
  make_sql_batch_by_piece_base(DataBase, Sql, Format, List, PieceNum, 0, "");
make_sql_batch_by_piece_base(DataBase, Sql, Format, [E | List], PieceNum, AccNum, Acc) ->
  Acc2 = "," ++ io_lib:format(Format, E) ++ Acc,
  make_sql_batch_by_piece_base(DataBase, Sql, Format, List, PieceNum, AccNum + 1, Acc2).


to_term(Bin) ->
  to_term(Bin, []).
to_term(Bin, Default) ->
  case catch binary_to_term(Bin) of
    {'EXIT', _} ->
      Default;
    Term ->
      Term
  end.

to_bin(Term) ->
  quote(term_to_binary(Term)).

compress_encode(Term) ->
  zlib:compress(term_to_binary(Term)).

uncompress_decode(Bin) ->
  uncompress_decode(Bin, []).
uncompress_decode(Bin, Default) ->
  case catch binary_to_term(zlib:uncompress(Bin)) of
    {'EXIT', _} ->
      Default;
    Term ->
      Term
  end.

datetime({{A, B, C}, {D, E, F}}) ->
  io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w", [A, B, C, D, E, F]);
datetime(Err) ->
  ?ERR_WITH_STACK("datetime err:~p~n", [Err]).
date({A, B, C}) ->
  io_lib:format("~w-~.2.0w-~.2.0w", [A, B, C]);
date(Err) ->
  ?ERR_WITH_STACK("date err:~p~n", [Err]).
time({A, B, C}) ->
  io_lib:format("~.2.0w:~.2.0w:~.2.0w", [A, B, C]).
minute({A, B, _C}) ->
  io_lib:format("~.2.0w:~.2.0w", [A, B]).


bool2int(true) ->
  1;
bool2int(false) ->
  0.

int2bool(1) ->
  true;
int2bool(0) ->
  false.

quote(String) when is_list(String) ->
  [39 | lists:reverse([39 | quote(String, [])])];    %% 39 is $'
quote(Bin) when is_binary(Bin) ->
  list_to_binary(quote(binary_to_list(Bin))).

quote([], Acc) ->
  Acc;
quote([0 | Rest], Acc) ->
  quote(Rest, [$0, $\\ | Acc]);
quote([10 | Rest], Acc) ->
  quote(Rest, [$n, $\\ | Acc]);
quote([13 | Rest], Acc) ->
  quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
  quote(Rest, [$\\, $\\ | Acc]);
quote([39 | Rest], Acc) -> %% 39 is $'
  quote(Rest, [39, $\\ | Acc]);    %% 39 is $'
quote([34 | Rest], Acc) -> %% 34 is $"
  quote(Rest, [34, $\\ | Acc]);    %% 34 is $"
quote([26 | Rest], Acc) ->
  quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
  quote(Rest, [C | Acc]).

log_dup_table(Suffix, Table) ->
  Sql = "create table if not exists " ++ Table ++ Suffix ++ " like " ++ Table ++ ";",
  sql_execute_with_log(?db, Sql).

log_dup_tables(Suffix) ->
  log_dup_table(Suffix, "t_add_vip")
  , log_dup_table(Suffix, "t_buy_coin").

log_create_next() ->
  {Year, Month, Day} = Date = erlang:date(),
  LastDay = calendar:last_day_of_the_month(Year, Month),
  case Day of
    LastDay ->
      {NewY, NewM, _} = util:day_add(Date, 1),
      case erlang:get(?log_table_next) of
        {NewY, NewM, _} ->
          ignore;
        _ ->
          Suffix = "_" ++ integer_to_list(NewY) ++ "_" ++ integer_to_list(NewM),
          erlang:put(?log_table_next, {NewY, NewM, Suffix}),
          log_dup_tables(Suffix)
      end;
    _ ->
      ignore
  end.

log_create_tables() ->
  Date = erlang:date(),
  log_create_tables(Date).

log_create_tables(Date) ->
  {Year, Month, _} = Date,
  case erlang:get(?log_table_time) of
    {Year, Month, S} ->
      S;
    _ ->
      Suffix = "_" ++ integer_to_list(Year) ++ "_" ++ integer_to_list(Month),
      erlang:put(?log_table_time, {Year, Month, Suffix}),
      log_dup_tables(Suffix),
      {MoreY, MoreM, _} = util:day_add(Date, calendar:last_day_of_the_month(Year, Month)),
      SuffixMore = "_" ++ integer_to_list(MoreY) ++ "_" ++ integer_to_list(MoreM),
      log_dup_tables(SuffixMore),
      Suffix
  end.


del_log() ->
  lists:foreach(fun([Tabname]) ->
    Tabname2 = binary_to_list(Tabname),
    case re:run(Tabname2, "_2015_") of
      {match, _} ->
        io:format("delete table: ~s~n", [Tabname2]),
        DelSql = io_lib:format("drop table ~s;", [Tabname2]),
        sql_execute_with_log(DelSql);
      _ -> ignore
    end
  end, get_all("show tables")).

log_create_role([]) ->
  {ok, 0};
log_create_role(List) ->
  ArgList = [[Accid, quote(DevID), Srctype, quote(AccName), RoleID, MacAddr, IP, Result, date(Date), time(Time), Sex]
    || {Accid, DevID, Srctype, AccName, RoleID, MacAddr, IP, Sex, Result, Date, Time} <- List],
  make_sql_log_batch_by_piece("insert into t_create_role values",
    "(~w,~s,~w,~s,~w,~s,'~w',~w,'~s','~s', ~w)", ArgList, 1000).
