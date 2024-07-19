%% Feel free to use, reuse and abuse the code in this file.

-module(gate).
-behaviour(gen_statem).
-behaviour(ranch_protocol).

%% API.
-export([start_link/3]).

%% gen_statem.
-export([callback_mode/0]).
-export([init/1]).
-export([connected/3]).
-export([terminate/3]).
-export([code_change/4]).

-include("common.hrl").
-include("all.hrl").
-include("errcode.hrl").

-define(TIMEOUT, 60000).

%%可为下面三种模式
-define(ACTIVE_MODE, {active, once}).

%%active true 非阻塞式
%%当一个主动套接字被创建后，它会在收到数据时向控制进程发送{tcp, Socket, Data}
%%消息。控制进程无法控制这些消息流。恶意的客户端可以向系统发送成千上万的消息，
%%而它们都会被发往控制进程。控制进程无法阻止这些消息流
%%
%%active false 阻塞式
%%如果一个套接字是用被动模式打开的，控制进程就必须调用gen_tcp:recv(Socket, N)
%%来从这个套接字接收数据。然后它会尝试从套接字接收N个字节。如果N = 0，套接字就
%%会返回所有可用的字节
%%调用prim_inet:async_recv，返回回调 {inet_async, Socket, _Ref, {ok, Data}}
%%
%%active once 部分阻塞式
%%套接字在这个模式下虽然是主动的，但只针对一个消息。当控制进程收到一个消
%%息后，必须显式调用inet:setopts才能重启下一个消息的接收，在此之前系统会处于阻塞状态
-define(TCP_OPTIONS, [
  binary
  , {packet, 4}
  , {reuseaddr, true}
  , {nodelay, false}
  , {delay_send, true}
  , {send_timeout, 15000}
  , {keepalive, true}
  , {exit_on_close, true}
]).

-record(state, {
  ref
  , transport
  , socket
  , rolePid
  , roleID
}).

%% API.

start_link(Ref, Transport, Opts) ->
%%  这里不register gw
  gen_statem:start_link(?MODULE, {Ref, Transport, Opts}, []).

%% gen_statem.

callback_mode() ->
  [state_functions, state_enter].

init({Ref, Transport, _Opts = []}) ->
  process_flag(trap_exit, true),
%%  {ok, connected, #state{ref = Ref, transport = Transport}, ?TIMEOUT}. 超时关闭
  {ok, connected, #state{ref = Ref, transport = Transport, socket = 0, rolePid = 0, roleID = 0}, infinity}.

connected(enter, connected, StateData = #state{
  ref = Ref, transport = Transport}) ->
  {ok, Socket} = ranch:handshake(Ref),
  lager:info("connected from ip:~p", [util:get_ip(Socket)]),
  ok = Transport:setopts(Socket, [?ACTIVE_MODE | ?TCP_OPTIONS]),
  case ?ACTIVE_MODE of
    {active, false} ->
      %%在send之前一定要调用一次，下次才能继续send
      prim_inet:async_recv(Socket, 0, -1);
    _ ->
      ignore
  end,
  {keep_state, StateData#state{socket = Socket}};
connected(info, {inet_async, Socket, _Ref, {ok, Data}}, StateData) ->
  lager:info("inet_async"),
  case ?ACTIVE_MODE of
    {active, false} ->
      %%在send之前一定要调用一次，下次才能继续send
      prim_inet:async_recv(Socket, 0, -1);
    _ ->
      ignore
  end,
  handle_messages(Data, StateData);
connected(info, {inet_async, _Socket, _Ref, {error, _Reason}}, State) ->
  lager:info("inet_async error"),
  {stop, normal, State};
connected(info, {tcp, Socket, Data}, StateData = #state{
  socket = Socket, transport = Transport}) ->
  lager:info("tcp socket data coming"),
  ok = Transport:setopts(Socket, [?ACTIVE_MODE | ?TCP_OPTIONS]),
  handle_messages(Data, StateData);
%%   {keep_state_and_data, ?TIMEOUT};
connected(info, {tcp_closed, _Socket}, _StateData) ->
  lager:info("tcp_closed"),
  {stop, normal};
connected(info, {tcp_error, _, Reason}, _StateData) ->
  lager:info("tcp_error"),
  {stop, Reason};
connected({call, _From}, {send_client, Msg}, _StateData = #state{socket = Socket, transport = Transport}) ->
  lager:info("send_client"),
  Transport:send(Socket, Msg),
  keep_state_and_data;
connected({call, From}, {kick, Type}, _StateData = #state{socket = _Socket, roleID = RoleID}) ->
  Reason =
    if is_integer(Type) ->
      Type;
      Type =:= login_again ->
        ?err_kick_login_again;
      Type =:= forbidden ->
        ?err_kick_ban;
      Type =:= too_fast ->
        ?err_kick_heart_fast;
      true ->
        ?err_kick_other
    end,
%% todo send msg to client
  lager:info("kick RoleID ~p ~p Reason ~p", [RoleID, Type, Reason]),
  gen_statem:reply(From, ok),
  {stop, normal};
connected({call, From}, _Request, _StateData) ->
  lager:info("call"),
  gen_statem:reply(From, ok),
  keep_state_and_data;
connected(cast, _Msg, _StateData) ->
  lager:info("cast"),
  keep_state_and_data;
connected(timeout, _Msg, _StateData) ->
  lager:info("timeout"),
  {stop, normal};
%%todo gc
%%handle_info({timeout, TimerRef, heart_timer}, #state{active_time = ActiveTime} = State) ->
%%  case State#state.heart_timer of
%%    TimerRef ->
%%      NowMili = util:now_mili(),
%%      case erlang:get(lastGCMili) of
%%        ?undefined ->
%%          erlang:put(lastGCMili, NowMili);
%%        LastGC ->
%%          case NowMili - LastGC > 60000 of
%%            true ->
%%              erlang:put(lastGCMili, NowMili),
%%              erlang:put(total_inter, 0),
%%              erlang:garbage_collect(self());
%%            false ->
%%              ignore
%%          end
%%      end,
%%      case ActiveTime + ?WAITING_HEART < util:now_mili() of
%%        true ->
%%          ?INFO("stop for timeout"),
%%          {stop, normal, State};
%%        _ ->
%%          {noreply, State#state{heart_timer = heart_timer()}}
%%      end;
%%    _ ->
%%      {noreply, State#state{heart_timer = heart_timer()}}
%%  end;
connected(EventType, Msg, _StateData) ->
  lager:info("other EventType ~p Msg ~p", [EventType, Msg]),
  {stop, normal}.
terminate(Reason, StateName, StateData = #state{
  socket = Socket, transport = Transport})
  when Socket =/= undefined, Transport =/= undefined ->
  lager:info("terminate"),
  catch Transport:close(Socket),
  terminate(Reason, StateName,
    StateData#state{socket = undefined, transport = undefined});
terminate(_Reason, _StateName, _StateData) ->
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

handle_messages(Data, StateData = #state{socket = Socket, roleID = RoleID}) ->
  Message = proto:decode(Data),
  lager:info("Message ~p", [Message]),
  case do_handle_messages(Message, StateData) of
    #state{} = NewStateData ->
      {keep_state, NewStateData};
    {not_login, _} ->
      {stop, normal};
    {login_fail, _} ->
      {stop, normal};
    {ip_banned, _} ->
      {stop, normal};
    {Error, _} ->
      lager:error("stop for ~10000p, Bin ~10000p  ~w   ~w", [Error, Data, util:get_ip(Socket), RoleID]),
      {stop, normal}
  end.

do_handle_messages({packet_error, Bin}, State) ->
  {{packet_error, Bin}, State};
do_handle_messages(Message, State) ->
  case ?LOOSE_CATCH(gate_route(Message, State)) of
    #state{} = NewState ->
      NewState;
    {Error, NewState} ->
      {Error, NewState}
  end.

gate_route(Message, #state{socket = Socket, transport = Transport, rolePid = RolePid} = State) ->
  Ip = util:get_ip(Socket),
  case allow_ip(Ip) of
    true ->
      case Message of
        #cs_heart{} ->
          cs_heart(Message, State);
        #cs_login{} ->
          cs_login(Message, State);
        _ ->
          case RolePid =/= 0 of
            true ->
              route_msg(Message, State);
            _ ->
              not_login
          end,
          State
      end;
    _ ->
      Transport:send(Socket, #sc_login{errcode = ?err_login_ip_banned}),
      ip_banned
  end.

%%消息路由
route_msg(Message, State) ->
  Name = element(1, Message),
  case route:route(Name) of
    {role, Module} ->
      lager:info("route_msg rolePid ~p", [State#state.rolePid]),
      erlang:send(State#state.rolePid, {client_msg, Module, Message});
    {Server, _HandleModule} ->
      catch erlang:send(Server, {client_msg, State#state.roleID, Message})
  end.

allow_ip(_Ip) ->
%%  todo
  true.

cs_heart(_Message, #state{socket = Socket, transport = Transport} = State) ->
  Transport:send(Socket, #sc_heart{timestamp = util:now()}),
  State.

cs_login(#cs_login{username = Username, password = Password}, #state{socket = Socket, transport = Transport} = State) ->
%%todo 最大登录数限制等
%%  mnesia是根据主键去更新记录的，如果主键不存在则插入，table第一个field为主键
  RoleID =
    case gate_mnesia:valid(Username, Password) of
      [Id] ->
        Id;
      [] ->
        gate_mnesia:insert(Username, Password)
    end,

%%  是否重复登录gw
  GwName = role_lib:gate_name(RoleID),
  case role_lib:gw(RoleID) of
    ?undefined ->
      erlang:register(GwName, self());
    OldPid ->
      %%老玩家所在gate处理通知玩家下线，gw回收
      catch kick(OldPid, login_again),
      erlang:unregister(GwName),
      erlang:register(GwName, self())
  end,
  DevID = 0,
%%  是否有pid进程，pid进程重用
  case role_lib:pid(RoleID) of
    ?undefined ->
      {ok, Pid} = role:start([RoleID, self(), Socket, Transport, DevID]);
    Pid ->
      erlang:send(Pid, {login_again, self(), Socket, Transport, DevID})
  end,
  Transport:send(Socket, proto:encode(#sc_login{errcode = ?err_ok})),
  State#state{rolePid = Pid}.

kick(Gw, Type) when is_pid(Gw) ->
  gen_statem:call(Gw, {kick, Type});
kick(RoleID, Type) when is_integer(RoleID) ->
  Gw = role_lib:gw(RoleID),
  kick(Gw, Type);
kick(_, _) ->
  ok.