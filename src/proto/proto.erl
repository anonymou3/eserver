%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 6月 2022 15:00
%%%-------------------------------------------------------------------
-module(proto).
-author("new").

%% API
-export([encode/1, decode/1]).

encode(Record) ->
  Name = element(1, Record),
  Bin = all:encode_msg(Record, Name),
  Id = route:get_id(Name),
  A = list_to_binary([<<Id:32>>, Bin]),
  lager:info("encode ~s ~p", [Name, A]),
  A.

decode(<<ID:32, Bin/binary>>) ->
  try
    Name = route:get_name(ID),
    A = all:decode_msg(Bin, Name),
    lager:info("decode ~s ~p", [Name, A]),
    A
  catch
    _Error: _Reason ->
      {packet_error, Bin}
  end.