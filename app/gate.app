%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 5月 2022 16:09
%%%-------------------------------------------------------------------
{application, gate, [
  {description, "gate"},
  {vsn, "1"},
  {modules, [gate, gate_listener_app, gate_listener_sup]},
  {registered, [gate_listener_sup]},
  {applications, [
    kernel,
    stdlib,
    ranch
  ]},
  {mod, {gate_listener_app, []}},
  {env, []}
]}.