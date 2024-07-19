-module(config_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
	| one_for_one
	| rest_for_one
	| simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
	| transient
	| temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
	AChild = {config, {config, start_link,[]},
		permanent,2000,worker,[config]},
	{ok,{{one_for_one,10,10}, [AChild]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


