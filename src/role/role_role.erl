-module(role_role).
-compile(export_all).
-include("def_role.hrl").

get_misc(Key, Default) ->
  RoleMisc = role_data:get_role_misc(),
  case catch dict:find(Key, RoleMisc#role_misc.dictData) of
    {ok, Value} -> Value;
    _ ->
      Default
  end.

get_misc(RoleID, Key, Default) ->
  RoleMisc = role_data:get_role_misc(RoleID),
  case catch dict:find(Key, RoleMisc#role_misc.dictData) of
    {ok, Value} -> Value;
    _ ->
      Default
  end.

set_misc(Key, Value) ->
  RoleMisc = role_data:get_role_misc(),
  NewDictData = dict:store(Key, Value, RoleMisc#role_misc.dictData),
  role_data:set_role_misc(RoleMisc#role_misc{dictData = NewDictData}).

%%%%usage
%%get_data() ->
%%  role_role:get_misc(?type_misc_newdungeon, #r_tower{}).
%%
%%set_data(Infos) ->
%%  role_role:set_misc(?type_misc_newdungeon, Infos).
%%
%%clear_data() ->
%%  role_role:set_misc(?type_misc_newdungeon, #r_tower{}).