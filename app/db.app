%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 6月 2022 19:42
%%%-------------------------------------------------------------------
{application, db, [
  {description, "database application"},
  {vsn, "1"},
  {registered, []},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {db_app, []}},
  {env, []}
]}.