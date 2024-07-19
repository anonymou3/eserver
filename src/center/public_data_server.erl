%%公共数据服务
%%%-------------------------------------------------------------------
-module(public_data_server).

-behaviour(gen_server).
-include("def_role.hrl").

-compile(export_all).
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================


i() ->
  gen_server:call(?MODULE, i).

start() ->
  {ok, _} =
    supervisor:start_child(world_sup,
      {?MODULE,
        {?MODULE, start_link, []},
        transient, 600000, worker, [?MODULE]}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  random:seed(util:gen_random_seed()),
  process_flag(trap_exit, true),
  tk_misc:gc_self_work(),
  timer_wheel:init(),
  case util_master:is_master() of
    true ->
      %%master服加载全局数据
      lists:foreach(fun(E) ->
        Data = db_sql:get_public_data(E),
        erlang:put(E, Data)
      end, ?master_public_list);
    _ ->
      %%normal服加载全局数据
      lists:foreach(fun(E) ->
        Data = db_sql:get_public_data(E),
        erlang:put(E, Data)
      end, ?normal_public_list),

      %%normal服启动执行
      util_master:master_send(public_data_server, {normal_server_data_sync_master, ?master_public_month_fund, [], 0}) %请求master服务器同步一次
  end,
  {ok, #state{}}.

handle_call(i, _From, State) ->
  {reply, State, State};
handle_call(OriginRequest, _From, State) ->
  case util:util_handle_call(OriginRequest) of
    ?util_call_miss ->
      {reply, ?util_call_miss, State};
    Request ->
      Reply = ?TRY_CATCH(do_handle_call(Request)),
      {reply, Reply, State}
  end.

handle_cast(Msg, State) ->
  lager:error("handle_cast function clause:request=~100p", [Msg]),
  {noreply, State}.

handle_info({'EXIT', _From, Reason}, State) ->
  lager:error("~p  exit ,reason ~p", [?MODULE, Reason]),
  {stop, Reason, State};

handle_info(Info, State) ->
  ?CATCH(do_handle_info(Info)),
  {noreply, State}.

terminate(Reason, State) ->
  persist_data(),
  lager:error("~w terminate for \nReason=~300p\nState=~w\nDictionary=~1000p",
    [?MODULE, Reason, State, element(2, process_info(self(), dictionary))]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

do_handle_call({func, M, F, A}) ->
  apply(M, F, A);
do_handle_call(Request) ->
  lager:error("public_data_server server call unexcept msg=~w", [Request]).
%%
do_handle_info({normal_server_data_sync_master, Key, Common, Data}) ->
  lager:error("lala normal_server_data_sync_master, Key ~p, Common, Data ~p", [Key, Data]),
  normal_server_data_sync_master(Key, Common, Data);
do_handle_info({normal_server_data_sync, Key, Common, Data}) ->
  lager:error("lala normal_server_data_sync, Key ~p, Common, Data ~p", [Key, Data]),
  normal_server_data_sync(Key, Common, Data);
do_handle_info({set_normal_cache_key, Data}) ->
  ets:insert(?ets_public_data, Data);
do_handle_info({timer_wheel_tick, LastTick}) ->
  timer_wheel:work(LastTick),
  loop(LastTick);
do_handle_info(Msg) ->
  lager:error("public_data_server server info unexcept msg=~w", [Msg]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
persist_data() ->
  case util_master:is_master() of
    true ->
      lists:foreach(fun(E) ->
        Data = erlang:get(E),
        db_sql:set_public_data(E, Data)
      end, ?master_public_list);
    _ ->
      lists:foreach(fun(E) ->
        Data = erlang:get(E),
        db_sql:set_public_data(E, Data)
      end, ?normal_public_list)
  end,
  ok.

%%秒循环
loop(Now) ->
  ?TRY_CATCH(loop_persist(Now), Err1).

%%定时持久化数据
loop_persist(Now) ->
  case (Now rem 300) =:= 0 of %%定时持久化
    true ->
      persist_data();
    false ->
      ignore
  end.

normal_server_data_sync_master(Key, _Common, Data) ->
  Now =
    case erlang:get(Key) of
      [] ->
        0;
      Num ->
        Num
    end,
  New = Now + Data,
  erlang:put(Key, New),
  ?TRY_CATCH(util_master:all_normal_send(?MODULE, {set_normal_cache_key, {Key, New}}), Err2).

%%master服和normal服共用数据获取
get_data_list(Key) ->
  case ets:lookup(?ets_public_data, Key) of
    [{Key, Value}] ->
      Value;%%返回list
    _ ->
      []
  end.
get_data(Key) ->
  case ets:lookup(?ets_public_data, Key) of
    [{Key, Value}] ->
      Value;
    _ ->
      0
  end.


%%normal服数据记录
%%list    Key = [{Subkey,Val},...]
%%integer Key = Val
normal_server_data_sync(Key, _Common, Data) when is_record(Data, r_public_data) ->
  Now = erlang:get(Key),
  #r_public_data{subKey = SubKey, val = Val} = Data,
  New =
    case lists:keytake(SubKey, #r_public_data.subKey, Now) of
      false ->
        [#r_public_data{subKey = SubKey, val = Val} | Now];
      {value, #r_public_data{val = Old}, ResList} ->
        [#r_public_data{subKey = SubKey, val = Val + Old} | ResList]
    end,
  erlang:put(Key, New),
  erlang:send(self(), {set_normal_cache_key, {Key, New}});
normal_server_data_sync(Key, _Common, Data) when is_integer(Data) ->
  Now =
    case erlang:get(Key) of
      [] ->
        0;
      Num ->
        Num
    end,
  New = Now + Data,
  erlang:put(Key, New),
  erlang:send(self(), {set_normal_cache_key, {Key, New}}).


%%%%usage
%%send_master() ->
%%  #role{roleID = RoleID, roleName = RoleName} = role_data:get_roleInfo(),
%%  Common = #master_common{roleID = RoleID, roleName = util:to_string(RoleName), serverID = util_master:get_self_server_id()},
%%  util_master:master_send(public_data_server, {normal_server_data_sync_master, ?master_public_month_fund, Common, 1}).
%%
%%server_time() ->
%%  public_data_server:get_data(?master_public_month_fund).
