%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 6月 2022 14:50
%%%-------------------------------------------------------------------
-module(base).
-author("new").
-compile(export_all).

-include("all.hrl").
-include("common.hrl").

%% API
-export([]).

cs_base_info(#cs_base_info{}) ->
  lager:info("cs_base_info enter"),
  ?push(#sc_base_info{roleID = 1}).
