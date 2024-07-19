-module(role_data).
-compile(export_all).

-include("def_role.hrl").
-include("def_pflag.hrl").

get_role_misc() ->
  get_role_misc(role_data:get_roleID()).

get_role_misc(RoleID) ->
  ETS = ?ets_role_misc,
  case ets:lookup(ETS, RoleID) of
    [] ->
      RoleMisc = db_sql:get_role_misc(RoleID),
      ets:insert(ETS, RoleMisc),
      RoleMisc;
    [#role_misc{} = RoleMisc | _] ->
      RoleMisc
  end.

set_role_misc(RoleMisc) when is_record(RoleMisc, role_misc) ->
  put(?pflag_role_misc, true),
  ets:insert(?ets_role_misc, RoleMisc).
