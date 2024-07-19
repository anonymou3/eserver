%% MySQL/OTP + Poolboy
%% Copyright (C) 2015 Raoul Hess
%%
%% This file is part of MySQL/OTP + Poolboy.
%%
%% MySQL/OTP + Poolboy is free software: you can redistribute it and/or modify it under
%% the terms of the GNU Lesser General Public License as published by the Free
%% Software Foundation, either version 3 of the License, or (at your option)
%% any later version.
%%
%% This program is distributed in the hope that it will be useful, but WITHOUT
%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
%% FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for
%% more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with this program. If not, see <https://www.gnu.org/licenses/>.

-module(mysql_poolboy_sup).

-behaviour(supervisor).
-export([start_link/0, init/1]).

-include("common.hrl").

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  db_sql:create_databases(),

  lager:info("now starting connect db!"),
  {GamePoolOptions, GameMySqlOptions} = get_pool_boy_option(?db),
  {LogPoolOptions, LogMySqlOptions} = get_pool_boy_option(?db_log),
  ChildSpecs = [
    %% MySQL pools
    mysql_poolboy:child_spec(?db, GamePoolOptions, GameMySqlOptions),
    mysql_poolboy:child_spec(?db_log, LogPoolOptions, LogMySqlOptions)
  ],
  {ok, {{one_for_one, 10, 10}, ChildSpecs}}.

get_pool_boy_option(Key) ->
  {GameIP, GamePort, GameUser, GamePassWord, GameDatabase, GameConnectNum} = data_setting:get(Key),
  HighGameConnectNum = GameConnectNum * 2,
  GamePoolOptions = [{size, GameConnectNum}, {max_overflow, HighGameConnectNum}],
  GameMySqlOptions = [{host, GameIP}, {port, GamePort}, {user, GameUser}, {password, GamePassWord}, {database, GameDatabase}],
  {GamePoolOptions, GameMySqlOptions}.