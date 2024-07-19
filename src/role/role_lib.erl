%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 6月 2022 13:42
%%%-------------------------------------------------------------------
-module(role_lib).
-author("new").
-include("common.hrl").
-include("record.hrl").
-compile(export_all).

%% API
-export([]).

gw(RoleID) ->
  whereis(gate_name(RoleID)).

gate_name(RoleID) ->
  util:to_atom("gw" ++ integer_to_list(RoleID)).

role_name(RoleID) ->
  util:to_atom("role" ++ integer_to_list(RoleID)).

pid(RoleID) ->
  whereis(role_name(RoleID)).

call_server(RoleID, Info) ->
  util:util_call(role_name(RoleID), Info).

push(Msg) ->
  Socket = get(?socket),
  Transport = get(?transport),
  Transport:send(Socket, proto:encode(Msg)).


%%normal服全局数据设置
set_normal_global(Type) ->
  set_normal_global(Type, 1).
set_normal_global(Type, Data) when is_integer(Data) ->
  #role{roleID = RoleID, roleName = RoleName} = role_data:get_roleInfo(),
  Common = #master_common{roleID = RoleID, roleName = util:to_string(RoleName), serverID = util_master:get_self_server_id()},
  erlang:send(public_data_server, {normal_server_data_sync, Type, Common, Data});
%%set_normal_global(Type, #r_public_data{subKey=ID,val=xx}) %%一个key下记录多个动态数据列表
set_normal_global(Type, Data) when is_record(Data, r_public_data) ->
  #role{roleID = RoleID, roleName = RoleName} = role_data:get_roleInfo(),
  Common = #master_common{roleID = RoleID, roleName = util:to_string(RoleName), serverID = util_master:get_self_server_id()},
  erlang:send(public_data_server, {normal_server_data_sync, Type, Common, Data}).
