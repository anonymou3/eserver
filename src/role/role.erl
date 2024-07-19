%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 6月 2022 17:21
%%%-------------------------------------------------------------------
-module(role).
-author("new").

-behaviour(gen_server).

%% API
-export([start_link/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-export([start/1]).
-export([stop/1]).

-define(SERVER, ?MODULE).

-record(role_state, {roleID}).

-include("common.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(RoleID :: term(), Gw :: term(), Socket :: term(), Transport :: term(), DevID :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(RoleID, Gw, Socket, Transport, DevID) ->
  lager:info("start_link RoleID ~p Gw ~p Socket ~p Transport ~p", [RoleID, Gw, Socket, Transport]),
  %%	这里不能使用{local, ?SERVER} 而由init中使用role_name来注册
  gen_server:start_link(?MODULE, [RoleID, Gw, Socket, Transport, DevID], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #role_state{}} | {ok, State :: #role_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([RoleID, Gw, Socket, Transport, DevID]) ->
  lager:info("init RoleID ~p Gw ~p Socket ~p DevID ~p", [RoleID, Gw, Socket, DevID]),
  process_flag(trap_exit, true),
  rand:seed(default),
  put_process(RoleID, Gw, Socket, Transport),
  erlang:link(Gw), %%role的pid与gw link，一个进程终止了，连接组中的进程组全终止

  %%  todo 载入玩家数据
  %%  register pid
  register(role_lib:role_name(RoleID), self()),
  ets:insert(?ets_role_online, {RoleID, true}),
  %% mysql数据初始化
  %% todo
  util:gc_self_work_random(),
  {ok, #role_state{roleID = RoleID}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #role_state{}) ->
  {reply, Reply :: term(), NewState :: #role_state{}} |
  {reply, Reply :: term(), NewState :: #role_state{}, timeout() | hibernate} |
  {noreply, NewState :: #role_state{}} |
  {noreply, NewState :: #role_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #role_state{}} |
  {stop, Reason :: term(), NewState :: #role_state{}}).
handle_call({dic, Key}, _From, State) ->
  Result = ?CATCH(erlang:get(Key)),
  {reply, Result, State};
handle_call(state, _From, State) ->
  {reply, State, State};
handle_call(OriginRequest, From, State) ->
  case util:util_handle_call(OriginRequest) of
    ?util_call_miss ->
      {reply, ?util_call_miss, State};
    Request ->
      case ?TRY_CATCH_2(do_handle_call(Request, From, State)) of
        {reply, _, _} = Reply ->
          Reply;
        _ ->
          {reply, ok, State}
      end
  end.
%%handle_call(_Request, _From, State = #role_state{}) ->
%%  {reply, ok, State}.

do_handle_call({func, F, Args}, _From, State) ->
  Result = apply(F, Args),
  {reply, Result, State};
do_handle_call({func, M, F, Args}, _From, State) ->
  Result = apply(M, F, Args),
  {reply, Result, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #role_state{}) ->
  {noreply, NewState :: #role_state{}} |
  {noreply, NewState :: #role_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #role_state{}}).
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Request, State = #role_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #role_state{}) ->
  {noreply, NewState :: #role_state{}} |
  {noreply, NewState :: #role_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #role_state{}}).
%%gate路由过来的客户端已解包的消息
handle_info({client_msg, M, A}, State) ->
  lager:info("recv roleID=~w, msg=~w", [State, {client_msg, M, A}]),
  F = element(1, A),
  ?LOOSE_CATCH(M:F(A)),
  {noreply, State};

handle_info({?route, Module, Info}, State) ->
  lager:info("by route recv msg=~w", [{?route, Module, Info}]),
  FuncName = element(1, Info),
  ?CATCH(Module:FuncName(Info)),
  {noreply, State};
handle_info({?self_route, Module, Info}, State) ->
  lager:info("by self_route recv msg=~w", [{?route, Module, Info}]),
  ?CATCH(Module:do_handle_info(Info)),
  {noreply, State};
handle_info({'EXIT', From, Reason}, #role_state{roleID = _RoleID} = State) ->
  if From =/= self() andalso Reason == login_again ->
    {noreply, State};
    true ->
      {noreply, State}
  end;
handle_info(Info, State = #role_state{}) ->
  ?CATCH(do_handle_info(Info, State)),
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #role_state{}) -> term()).
terminate(_Reason, _State = #role_state{roleID = RoleID}) ->
  catch erlang:unregister(role_lib:role_name(RoleID)),
  ets:delete(?ets_role_online, RoleID),
  ets:delete(?ets_role_misc, RoleID),

  %%todo 数据落地
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #role_state{},
    Extra :: term()) ->
  {ok, NewState :: #role_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #role_state{}, _Extra) ->
  {ok, State}.

start(Args) ->
  % io:format("role_server start process is ~w\n", [self()]),
  %%  https://www.erlang.org/doc/man/supervisor.html#start_child-2
  %%  start_child(SupRef, ChildSpec) -> startchild_ret()
  %%  Dynamically adds a child specification to supervisor SupRef, which starts the corresponding child process.
  %%  SupRef can be any of the following:
  %%    The pid
  %%    Name, if the supervisor is locally registered
  %%    {Name,Node}, if the supervisor is locally registered at another node
  %%    {global,Name}, if the supervisor is globally registered
  %%    {via,Module,Name}, if the supervisor is registered through an alternative process registry
  %%  ChildSpec must be a valid child specification (unless the supervisor is a simple_one_for_one supervisor; see below).
  %%  The child process is started by using the start function as defined in the child specification.
  %%  For a simple_one_for_one supervisor, the child specification defined in Module:init/1 is used,
  %%  and ChildSpec must instead be an arbitrary list of terms List.
  %%  The child process is then started by appending List to the existing start function arguments,
  %%  that is, by calling apply(M, F, A++List), where {M,F,A} is the start function defined in the child specification.

  supervisor:start_child(role_sup, Args).
%% 对于simple_one_for_one的supervisor，start => {'role', start_link, []}，这里的New = Args ++ [] apply(M,F,A)调用role:start_link(New这里被一项项的展开)，Module:init(New这里被一项项的展开)会被调用

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 重复登录
do_handle_info({login_again, Gw, Socket, Transport, _DevID}, #role_state{roleID = RoleID}) ->
  %%	todo 刷新某些数据
  lager:info("login_again"),
  put_process(RoleID, Gw, Socket, Transport),
  ets:insert(?ets_role_online, {RoleID, true}),
  erlang:link(Gw).

stop(Pid) when is_pid(Pid) ->
  supervisor:delete_child(role_sup, Pid),
  catch gen_server:cast(Pid, stop);
stop(RoleID) when is_integer(RoleID) ->
  stop(role_lib:pid(RoleID)).

put_process(RoleID, Gw, Socket, Transport) ->
  put(?roleID, RoleID),
  put(?gw, Gw),
  put(?socket, Socket),
  put(?transport, Transport).