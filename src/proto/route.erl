-module(route).
-compile(export_all).

%% get_name begin---------
get_name(100) ->
	cs_login;
get_name(101) ->
	sc_login;
get_name(102) ->
	cs_heart;
get_name(103) ->
	sc_heart;
get_name(200) ->
	cs_base_info;
get_name(201) ->
	sc_base_info;
get_name(_) ->
	undefined.


%% get_id begin---------
get_id(cs_login) ->
	100;
get_id(sc_login) ->
	101;
get_id(cs_heart) ->
	102;
get_id(sc_heart) ->
	103;
get_id(cs_base_info) ->
	200;
get_id(sc_base_info) ->
	201;
get_id(_) ->
	undefined.


%% route begin---------
route(cs_login) ->
	{gate,gate};
route(sc_login) ->
	{gate,gate};
route(cs_heart) ->
	{gate,gate};
route(sc_heart) ->
	{gate,gate};
route(cs_base_info) ->
	{role,base};
route(sc_base_info) ->
	{role,base};
route(_) ->
	undefined.
