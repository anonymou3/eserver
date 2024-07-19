-module(config).
-compile(export_all).
-behaviour(gen_server).
-include("preload_config.hrl").

-export([start_link/0, preload_config/0, root_dir/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([]).


-record(state, {}).

%% @doc 获取服务器根目录
root_dir() ->
  filename:dirname(filename:dirname(element(2, code:is_loaded(?MODULE)))).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc gen_server:init/1
-spec init(Args :: term()) -> Result when
  Result :: {ok, State}
  | {ok, State, Timeout}
  | {ok, State, hibernate}
  | {stop, Reason :: term()}
  | ignore,
  State :: term(),
  Timeout :: non_neg_integer() | infinity.

%% 此进程目前只做了初始化的事情，初始化完成后直接hibernate
init([]) ->
  preload_config(),
  erlang:garbage_collect(),
  {ok, #state{}, hibernate}.


%% @doc gen_server:handle_call/3
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
  Result :: {reply, Reply, NewState}
  | {reply, Reply, NewState, Timeout}
  | {reply, Reply, NewState, hibernate}
  | {noreply, NewState}
  | {noreply, NewState, Timeout}
  | {noreply, NewState, hibernate}
  | {stop, Reason, Reply, NewState}
  | {stop, Reason, NewState},
  Reply :: term(),
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity,
  Reason :: term().
handle_call(Request, _From, State) ->
  lager:error("handle_call function clause:request=~100p", [Request]),
  Reply = ok,
  {reply, Reply, State}.


%% @doc gen_server:handle_cast/2
-spec handle_cast(Request :: term(), State :: term()) -> Result when
  Result :: {noreply, NewState}
  | {noreply, NewState, Timeout}
  | {noreply, NewState, hibernate}
  | {stop, Reason :: term(), NewState},
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity.
handle_cast(Msg, State) ->
  lager:error("handle_cast function clause:request=~100p", [Msg]),
  {noreply, State}.


%% @doc gen_server:handle_info/2
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
  Result :: {noreply, NewState}
  | {noreply, NewState, Timeout}
  | {noreply, NewState, hibernate}
  | {stop, Reason :: term(), NewState},
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity.
handle_info(Info, State) ->
  lager:error("handle_info function clause:request=~100p", [Info]),
  {noreply, State}.

%% @doc gen_server:terminate/2
-spec terminate(Reason, State :: term()) -> Any :: term() when
  Reason :: normal
  | shutdown
  | {shutdown, term()}
  | term().
terminate(Reason, State) ->
  lager:error("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p", [Reason, State, element(2, process_info(self(), dictionary))]),
  ok.


%% @doc gen_server:code_change/3
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
  Result :: {ok, NewState :: term()} | {error, Reason :: term()},
  OldVsn :: Vsn | {down, Vsn},
  Vsn :: term().
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc 预加载配置
preload_config() ->
  load_configs(get_configs()).

load_configs(ConfigList) ->
  RootDir = root_dir(),
  OutDir = filename:join([RootDir, "ebin"]),
  lists:foreach(fun({ConfigPath, Module, Type, TransformFun}) ->
    io:format("load config ~s\n", [ConfigPath]),
    config2erl:beam(filename:join([RootDir, ConfigPath]), Module, OutDir, Type, TransformFun);
    (Fun) when is_function(Fun, 1) ->
      Fun(OutDir);
    (Fun) when is_function(Fun, 0) ->
      Fun()
  end, ConfigList).

get_configs() ->
  ?PRELOAD_CONFIG.

reload_config(ConfigMod) ->
  RootDir = root_dir(),
  OutDir = filename:join([RootDir, "ebin"]),
  case lists:keyfind(ConfigMod, 2, get_configs()) of
    false ->
      io:format("this config is new, please try reload like this: config:reload_config(\"config/data_skill.config\",data_skill,key_value,original)."),
      {reload_config_error, no_this_config, ConfigMod};
    {ConfigPath, Module, Type, TransformFun} ->
      case config2erl:beam(filename:join([RootDir, ConfigPath]), Module, OutDir, Type, TransformFun) of
        {ok, _CodeBin} ->
          c:l(Module),
          {reload_config_ok, ConfigMod};
        _ ->
          {reload_config_error, config_is_wrong, ConfigMod}
      end
  end.

reload_config(ConfigRelativePath, ModuleAtom, Type, TransformFun) ->
  RootDir = root_dir(),
  OutDir = filename:join([RootDir, "ebin"]),
  config2erl:beam(filename:join([RootDir, ConfigRelativePath]), ModuleAtom, OutDir, Type, TransformFun),
  c:l(ModuleAtom).
