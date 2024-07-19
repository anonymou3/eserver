%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 6月 2022 15:18
%%%-------------------------------------------------------------------
-module(gate_mnesia).
-author("new").

%% API
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("def_mnesia.hrl").

exist(Username) ->
  do(qlc:q([X#player.uuid || X <- mnesia:table(player),
    X#player.username =:= Username
  ])).

valid(Username, Password) ->
  do(qlc:q([X#player.uuid || X <- mnesia:table(player),
    X#player.username =:= Username,
    X#player.password =:= Password
  ])).

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

insert(Username, Password) ->
%%  gen uuid serverid >> 61 + count
  SId = 1,
  Header = SId bsl 61,
  Id = Header + count() + 1,
  Row = #player{username = Username, password = Password, uuid = Id, logintime = util:now(), logouttime = 0},
  F = fun() ->
    mnesia:write(Row)
      end,
  mnesia:transaction(F),
  Id.

show_all(Tab) ->
  do(qlc:q([X || X <- mnesia:table(Tab)])).

count() ->
  F = fun() ->
    mnesia:table_info(player, size)
      end,
  {atomic, Val} = mnesia:transaction(F),
  Val.