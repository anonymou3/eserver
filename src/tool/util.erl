%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 6月 2022 14:11
%%%-------------------------------------------------------------------
-module(util).
-include("common.hrl").

-export([
  pow/1
  , log/1
  , log/2
  , now/0
  , ms/0
  , md5/1
  , ceil/1
  , floor/1
  , sleep/1
  , sleep/2
  , get_list/2
  , implode/2
  , implode/3
  , explode/2
  , explode/3
  , for/3
  , for/4
  , string_to_term/1
  , bitstring_to_term/1
  , term_to_string/1
  , term_to_bitstring/1
  , to_string/1
  , null_proc/0
  , to_atom/1
  , toSqlDate/1
  , toUnixTime/1
  , datetime_to_seconds/1
  , seconds_to_datetime/1
  , foldl/3
  , nth_take/2
  , fun_take/2
  , fun_find/2
  , fun_replace/3
  , keymax/2
  , keymin/2
  , is_duplicate/1
  , copy_list/2
  , to_list/1
  , append_kvList/2
  , append_kvList/1
  , nth/2
  , element_pos/2
  , random_int/2
  , random_list/2
  , random_list2/2
  , random_list2/1
  , random_list3/2
  , random_weigh_list/2
  , random_weigh_list2/2
  , tc/2
  , catch_binary_to_term/1
  , ip_to_str/1
  , words_filter/1
  , words_filter_2/1
  , check_is_mul/2
  , check_blankName/1
  , calc_name_length/1
  , cal_utf8_length/1
  , gen_utf8_decode_list/2
  , random_str/1
  , filter_emoji/1
  , greater/2
  , lesss/2
  , jue_dui/1
  , loop_bu_zero/2
  , util_call/2
  , util_handle_call/1
  , check_reset_time/2
  , random_one_from_list/1
  , choose_one_from_list/2
  , choose_one_from_limit/2
  , choose_reward_from_list/2
  , check_day_of_week/1
  , check_day_of_month/1
  , check_dirty_words/0
  , max_list/1
  , min_list/1
  , ip_ntoa/1
  , delSame/2
  , is_same_week/1
  , is_same_week/2
  , nth_week/0
  , nth_week/1
  , http_build_query/1
  , keysum/2
  , shuffle/1
  , sqlDayToDate/1
  , dateToSqlDay/1
  , int_format_two/1
  , read_timer/1
  , ets_foreach_key/2
  , ets_all_key/1
  , count_src_line_num/1
  , gen_random_seed/0
  , latin1/1
  , is_name_en/1
  , datetime_shift/1
  , datetime_unshift/1
  , day_diff/0
  , day_diff/1
  , day_diff/2
  , day_between_secs/2
  , get_ip/1
  , if3/3
  , gc_self_work/0
  , gc_self_work_random/0
]).

-define(GREGORIAN_INTERVAL_TIME, (calendar:datetime_to_gregorian_seconds(data_setting:get(time_zone)))).

%% 2的N次方
pow(N) ->
  pow(2, N).
%% pow(0,R) ->
%%     R;
%% pow(N,R)when N > 0 ->
%%     pow(N-1,2*R).

%% Num的N次方
pow(Num, N) ->
  pow(Num, N, 1).

pow(_Num, 0, Result) ->
  Result;
pow(Num, N, Result) ->
  pow(Num, N - 1, Result * Num).

%% 在List中的每两个元素之间插入一个分隔符
implode(_S, []) ->
  [<<>>];
implode(S, L) when is_list(L) ->
  implode(S, L, []).
implode(_S, [H], NList) ->
  lists:reverse([thing_to_list(H) | NList]);
implode(S, [H | T], NList) ->
  L = [thing_to_list(H) | NList],
  implode(S, T, [S | L]).

%% 字符->列
explode(S, B) ->
  re:split(B, S, [{return, list}]).
explode(S, B, int) ->
  [list_to_integer(Str) || Str <- explode(S, B), length(Str) > 0].

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X) -> float_to_list(X);
thing_to_list(X) when is_atom(X) -> atom_to_list(X);
thing_to_list(X) when is_binary(X) -> binary_to_list(X);
thing_to_list(X) when is_list(X) -> X.

%% 日志记录函数
%% log(T, F, A, Mod, Line) ->
%%     {ok, Fl} = file:open("logs/error_log.txt", [write, append]),
%%     Format = list_to_binary("#" ++ T ++" ~s[~w:~w] " ++ F ++ "\r\n~n"),
%%     {{Y, M, D},{H, I, S}} = erlang:localtime(),
%%     Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
%%     io:format(Fl, unicode:characters_to_list(Format), [Date, Mod, Line] ++ A),
%%     file:close(Fl).

log(Format, Args) ->
  lager:error(Format, Args).

log(Format) ->
  lager:error(Format).

%% @doc 取得当前的unix时间戳,单位：秒
now() ->
  {M, S, _} = os:timestamp(),
  M * 1000000 + S.

%% @doc 当前时间戳，单位：毫秒
ms() ->
  {M, S, Ms} = os:timestamp(),
  M * 1000000000 + S * 1000 + Ms div 1000.

%% 转换成HEX格式的md5
md5(S) ->
  lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(S))]).

%%向上取整
ceil(N) when is_float(N) ->
  FN = erlang:floor(N),
  D = N - FN,
  case D > 0.02 of
    true ->
      FN + 1;
    false ->
      FN
  end;
ceil(N) ->
  N.

%%向下取整
floor(X) ->
  T = trunc(X),
  case (X < T) of
    true -> T - 1;
    _ -> T
  end.

sleep(T) ->
  receive
  after T -> ok
  end.

sleep(T, F) ->
  receive
  after T -> F()
  end.

get_list([], _) ->
  [];
get_list(X, F) ->
  F(X).

%% for循环

for(Max, Max, F) ->
  F(Max);
for(I, Max, F) when I =< Max ->
  F(I),
  for(I + 1, Max, F);
for(_I, _Max, _F) ->
  nil.

%% 带返回状态的for循环
%% @return {ok, State}
for(Max, Min, _F, State) when Min < Max -> {ok, State};
for(Max, Max, F, State) -> F(Max, State);
for(I, Max, F, State) -> {ok, NewState} = F(I, State), for(I + 1, Max, F, NewState).

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
  binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
  erlang:list_to_bitstring(io_lib:format("~p", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
  case erl_scan:string(String ++ ".") of
    {ok, Tokens, _} ->
      case erl_parse:parse_term(Tokens) of
        {ok, Term} -> Term;
        _Err -> undefined
      end;
    _Error ->
      undefined
  end.

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
  string_to_term(binary_to_list(BitString)).

to_string(Term) when is_atom(Term) ->
  atom_to_list(Term);
to_string(Term) when is_integer(Term) ->
  integer_to_list(Term);
to_string(Term) when is_binary(Term) ->
  binary_to_list(Term);
to_string(Term) when is_list(Term) ->
  Term.

null_loop() ->
  receive
    _ ->
      null_loop()
  end.

null_proc() ->
  spawn(fun null_loop/0).

to_atom(A) when is_list(A) ->
  erlang:list_to_atom(A);
to_atom(A) when is_integer(A) ->
  to_atom(integer_to_list(A));
to_atom(A) when is_atom(A) ->
  A;
to_atom(A) when is_binary(A) ->
  to_atom(binary_to_list(A));
to_atom(A) ->
  [List] = io_lib:format("~w", [A]),
  erlang:list_to_atom(List).

datetime_to_seconds({_Date, _Time} = Datetime) ->
  calendar:datetime_to_gregorian_seconds(Datetime)
    - ?GREGORIAN_INTERVAL_TIME.

seconds_to_datetime(MTime) ->
  calendar:gregorian_seconds_to_datetime(
    ?GREGORIAN_INTERVAL_TIME + MTime).

toSqlDate({A, B, _C}) when A < 1900 ->
  UnixTime = A * 1000000 + B,
  toSqlDate(UnixTime);
toSqlDate({{A, B, C}, {D, E, F}}) ->
  A * 10000000000 + B * 100000000 + C * 1000000 + D * 10000 + E * 100 + F;
toSqlDate(UnixTime) when is_integer(UnixTime) ->
  {{A, B, C}, {D, E, F}} = seconds_to_datetime(UnixTime),
  A * 10000000000 + B * 100000000 + C * 1000000 + D * 10000 + E * 100 + F.

int_format_two(A) ->
  lists:flatten(io_lib:format("~2..0w", [A])).

dateToSqlDay({A, B, C}) ->
  integer_to_list(A) ++ "-" ++ int_format_two(B) ++ "-" ++ int_format_two(C).

sqlDayToDate(String) ->
  case string:tokens(String, "-") of
    [A, B, C] ->
      {list_to_integer(A), list_to_integer(B), list_to_integer(C)};
    _ ->
      {0, 0, 0}
  end.

toUnixTime({A, B, _C}) ->
  A * 1000000 + B;
toUnixTime({datetime, Time}) ->
  datetime_to_seconds(Time);
toUnixTime(SqlDate) ->
  A = SqlDate div 10000000000,
  Rest = SqlDate rem 10000000000,
  B = Rest div 100000000,
  Rest2 = Rest rem 100000000,
  C = Rest2 div 1000000,
  Rest3 = Rest2 rem 1000000,
  D = Rest3 div 10000,
  Rest4 = Rest3 rem 10000,
  E = Rest4 div 100,
  F = Rest rem 100,
  datetime_to_seconds({{A, B, C}, {D, E, F}}).


foldl(_F, {return, Acc}, _L) ->
  Acc;
foldl(F, Acc, [Tail | L]) ->
  foldl(F, F(Tail, Acc), L);
foldl(F, Acc, []) when is_function(F, 2) ->
  Acc.

-define(seg_size, 16).
-define(max_seg, 32).
-define(expand_load, 5).
-define(contract_load, 3).
-define(exp_size, (?seg_size * ?expand_load)).
-define(con_size, (?seg_size * ?contract_load)).
-record(dict,
{size = 0 :: non_neg_integer(), % Number of elements
  n = ?seg_size :: non_neg_integer(), % Number of active slots
  maxn = ?seg_size :: non_neg_integer(), % Maximum slots
  bso = ?seg_size div 2 :: non_neg_integer(), % Buddy slot offset
  exp_size = ?exp_size :: non_neg_integer(), % Size to expand at
  con_size = ?con_size :: non_neg_integer(), % Size to contract at
  empty :: tuple(), % Empty segment
  segs :: tuple()          % Segments
}).
-define(kv(K, V), [K | V]).      % Key-Value pair format
fold_dict(F, Acc, D) ->
  Segs = D#dict.segs,
  fold_segs(F, Acc, Segs, tuple_size(Segs)).

fold_segs(_, {return, Acc}, _, _) -> Acc;
fold_segs(F, Acc, Segs, I) when I >= 1 ->
  Seg = element(I, Segs),
  fold_segs(F, fold_seg(F, Acc, Seg, tuple_size(Seg)), Segs, I - 1);
fold_segs(F, Acc, _, 0) when is_function(F, 3) -> Acc.

fold_seg(_, {return, _} = Acc, _, _) -> Acc;
fold_seg(F, Acc, Seg, I) when I >= 1 ->
  fold_seg(F, fold_bucket(F, Acc, element(I, Seg)), Seg, I - 1);
fold_seg(F, Acc, _, 0) when is_function(F, 3) -> Acc.

fold_bucket(_, {return, _} = Acc, _) -> Acc;
fold_bucket(F, Acc, [?kv(Key, Val) | Bkt]) ->
  fold_bucket(F, F(Key, Val, Acc), Bkt);
fold_bucket(F, Acc, []) when is_function(F, 3) -> Acc.

to_list(A) when is_binary(A) ->
  binary_to_list(A);
to_list(A) when is_integer(A) ->
  integer_to_list(A);
to_list(A) when is_atom(A) ->
  atom_to_list(A);
to_list(A) when is_list(A) ->
  A.

append_kvList(List) ->
  lists:foldl(fun({K, V}, Acc) ->
    case lists:keytake(K, 1, Acc) of
      false ->
        [{K, V} | Acc];
      {value, {K, V1}, Acc1} ->
        [{K, V + V1} | Acc1]
    end
  end, [], List).

append_kvList(List1, List2) when length(List1) >= length(List2) ->
  append_kvList2(List2, List1);
append_kvList(List1, List2) ->
  append_kvList2(List1, List2).

append_kvList2([], List) ->
  List;
append_kvList2([KV | Rest], List) ->
  append_kvList2(Rest, insert_kv(KV, List)).

insert_kv({Key, Value}, List) ->
  case lists:keytake(Key, 1, List) of
    {value, {Key, OldValue}, List2} ->
      [{Key, OldValue + Value} | List2];
    false ->
      [{Key, Value} | List]
  end.

%% 超过范围返回最后一个，非列表时直接返回
nth(_N, [H]) -> H;
nth(1, [H | _]) -> H;
nth(N, [_ | T]) when N > 1 ->
  nth(N - 1, T);
nth(_N, H) ->
  H.

%% 从一个带权重的列表中随机选取SelectNum个元素，可以相同
random_weigh_list2(List, SelectNum) ->
  TotalWeigh = get_total_weigh(List, 0),
  random_weigh_list2(List, SelectNum, TotalWeigh, []).


random_weigh_list2(_List, 0, _, Result) ->
  Result;
random_weigh_list2(List, SelectNum, TotalWeigh, Result) ->
  Random = rand:uniform() * TotalWeigh,
  {Weigh, _} = Element =
    foldl(fun({UnitWeigh, _} = Ele, Acc) ->
      if UnitWeigh + Acc > Random ->
        {return, Ele};
        true ->
          Acc + UnitWeigh
      end
    end, 0, List),
  random_weigh_list2(List, SelectNum - 1, TotalWeigh - Weigh, [Element | Result]).


%% 从一个带权重的列表List中随机选取SelectNum个不同元素，结果列表顺序随机
%% weighed_list:  [{weigh1, value1}] return [{wight, value}]
random_weigh_list(List, SelectNum) ->
  Len = length(List),
  if Len =< SelectNum ->
    List;
    true ->
      TotalWeigh = get_total_weigh(List, 0),
      random_weigh_list(List, SelectNum, TotalWeigh, [])
  end.


random_weigh_list(_List, 0, _, Result) ->
  Result;
random_weigh_list(List, SelectNum, TotalWeigh, Result) ->
  Random = rand:uniform() * TotalWeigh,
  {Weigh, _} = Element =
    foldl(fun({UnitWeigh, _} = Ele, Acc) ->
      if UnitWeigh + Acc > Random ->
        {return, Ele};
        true ->
          Acc + UnitWeigh
      end
    end, 0, List),
  NewList = lists:delete(Element, List),
  random_weigh_list(NewList, SelectNum - 1, TotalWeigh - Weigh, [Element | Result]).

random_one_from_weigh_list(List) ->
  TotalWeigh = get_total_weigh(List, 0),
  Random = rand:uniform() * TotalWeigh,
  foldl(fun({UnitWeigh, E}, Acc) ->
    if UnitWeigh + Acc > Random ->
      {return, E};
      true ->
        Acc + UnitWeigh
    end
  end, 0, List).

random_one_from_list([]) ->
  [];
random_one_from_list([One]) ->
  One;
random_one_from_list(List) ->
  Value = rand:uniform(length(List)),
  lists:nth(Value, List).

get_total_weigh([], Weigh) ->
  Weigh;
get_total_weigh([{WeighUnit, _} | Rest], Weigh) ->
  get_total_weigh(Rest, Weigh + WeighUnit).

keysum(Key, List) ->
  keysum(Key, List, 0).

keysum(Key, [H | T], Sum) ->
  keysum(Key, T, Sum + erlang:element(Key, H));
keysum(_Key, [], Sum) ->
  Sum.

%%乱序一个列表
shuffle(L) ->
  Len = length(L),
  NL = lists:map(fun(X) -> {rand:uniform(Len), X} end, L),
  NLL = lists:sort(NL),
  [V || {_, V} <- NLL].

%% 从列表List中随机选取SelectNum个元素，组成新的列表，新列表的元素排列顺序与其在List中顺序相同（不会重复）
random_list(List, SelectNum) ->
  Len = length(List),
  if Len =< SelectNum ->
    List;
    true ->
      random_list(List, SelectNum, Len, [])
  end.


random_list(_, 0, _, Result) ->
  lists:reverse(Result);
random_list([Head | Rest], SelectNum, Len, Result) ->
  case rand:uniform() =< SelectNum / Len of
    true ->
      random_list(Rest, SelectNum - 1, Len - 1, [Head | Result]);
    false ->
      random_list(Rest, SelectNum, Len - 1, Result)
  end.

random_list3(List, SelectNum) ->
  Len = length(List),
  if Len =< SelectNum ->
    List;
    true ->
      random_list3(List, [], SelectNum, Len, [])
  end.
random_list3(Rest, Head, 0, _, Result) ->
  {lists:reverse(Head) ++ Rest, Result};
random_list3([Head | Rest], Heads, SelectNum, Len, Result) ->
  case rand:uniform() =< SelectNum / Len of
    true ->
      random_list3(Rest, Heads, SelectNum - 1, Len - 1, [Head | Result]);
    false ->
      random_list3(Rest, [Head | Heads], SelectNum, Len - 1, Result)
  end.

%% 将一个列表元素随机一遍
random_list2(List) ->
  Len = length(List),
  random_list2(List, Len, Len, []).
%% 从一个列表中随机抽取N个，顺序随机 ，N可以超过界限
random_list2(List, N) ->
  random_list2(List, N, length(List), []).

random_list2(_List, 0, _Length, Result) ->
  Result;
random_list2(List, N, Length, Result) ->
  if Length =:= 1 ->
    Select = hd(List),
    Rest = [],
    random_list2(Rest, N - 1, Length - 1, [Select | Result]);
    Length =:= 0 ->
      Result;
    true ->

      Rand = rand:uniform(Length),
      {value, Select, Rest} = nth_take(Rand, List),
      random_list2(Rest, N - 1, Length - 1, [Select | Result])
  end.

%% 随机选择几个，可以重复
random_list_dupl(List, Num) ->
  random_list_dupl(List, [], Num).

random_list_dupl(_List, ListAcc, 0) ->
  ListAcc;
random_list_dupl(List, ListAcc, Num) ->
  A = random_one_from_list(List),
  ListAcc2 = [A | ListAcc],
  random_list_dupl(List, ListAcc2, Num - 1).

%% 计算一个元素在一个列表或tuple中第一次出现的位置
%% 元素在集合中不存在时，返回0
element_pos(Element, List) when is_list(List) ->
  element_pos(Element, List, 1);
element_pos(Element, Tuple) when is_tuple(Tuple) ->
  element_pos(Element, tuple_to_list(Tuple), 1);
element_pos(_, _) ->
  0.

element_pos(Element, [Element | _Rest], Index) ->
  Index;
element_pos(Element, [_ | Rest], Index) ->
  element_pos(Element, Rest, Index + 1);
element_pos(_, [], _) ->
  0.

%% 从[Lower...Higher]包括边界的整数区间中随机一个数
random_int(Lower, Higher) when Lower =< Higher ->
  rand:uniform(Higher - Lower + 1) + Lower - 1;
random_int(Higher, Lower) ->
  random_int(Lower, Higher).

random_boolean(Rate) ->
  if
    Rate >= ?rate_max ->
      true;
    true ->
      Rate >= random_int(1, ?rate_max)
  end.

random_boolean(Rate, Base) ->
  if
    Rate >= Base ->
      true;
    true ->
      Rate >= random_int(1, Base)
  end.

fun_take(F, L) ->
  fun_take(F, L, []).

fun_take(F, [H | T], L) ->
  case F(H) of
    true ->
      {value, H, lists:reverse(L, T)};
    false ->
      fun_take(F, T, [H | L])
  end;
fun_take(_F, [], _L) -> false.

fun_find(_F, []) ->
  false;
fun_find(F, [E | Rest]) ->
  case F(E) of
    true ->
      E;
    _ ->
      fun_find(F, Rest)
  end.


fun_replace(F, [Tup | Tail], New) ->
  case F(Tup) of
    true ->
      [New | Tail];
    _ ->
      [Tup | fun_replace(F, Tail, New)]
  end;
fun_replace(_F, [], _) -> [].

%% 读timer
read_timer(Timer) when is_reference(Timer) ->
  case erlang:read_timer(Timer) of
    false ->
      0;
    A ->
      A
  end;
read_timer(_) ->
  0.

%% 判断列表中是否有重复项
is_duplicate(List) ->
  is_duplicate(List, []).

is_duplicate([], _) ->
  false;
is_duplicate([H | T], List) ->
  case lists:member(H, List) of
    true ->
      true;
    false ->
      is_duplicate(T, [H | List])
  end.

%% 将列表复制N分，并组合成一个新的列表
copy_list(List, N) ->
  copy_list(List, N, []).

copy_list(_List, 0, Result) ->
  Result;
copy_list(List, N, Result) ->
  copy_list(List, N - 1, List ++ Result).

%% 找到tupleList中某字段最大的tuple
%% return: {MaxKey, Tuple}
keymax(List, Pos) ->
  [Head | Tail] = List,
  HeadKey = element(Pos, Head),
  lists:foldl(fun(E, {MaxKey, Tuple}) ->
    Key = element(Pos, E),
    if Key > MaxKey ->
      {Key, E};
      true ->
        {MaxKey, Tuple}
    end
  end, {HeadKey, Head}, Tail).

%% 找到tupleList中某字段最大的tuple
%% return: {MinKey, Tuple}
keymin(List, Pos) ->
  [Head | Tail] = List,
  HeadKey = element(Pos, Head),
  lists:foldl(fun(E, {MaxKey, Tuple}) ->
    Key = element(Pos, E),
    if Key < MaxKey ->
      {Key, E};
      true ->
        {MaxKey, Tuple}
    end
  end, {HeadKey, Head}, Tail).

%% 删除第N个，并返回新列表
%% return: {value, NthVar, NewList} | false
nth_take(N, List) ->
  nth_take(N, List, []).
nth_take(1, [NthVar | Tail], Temp) ->
  {value, NthVar, lists:reverse(Temp, Tail)};
nth_take(_N, [], _Temp) ->
  false;
nth_take(N, [Hd | Tail], Temp) ->
  nth_take(N - 1, Tail, [Hd | Temp]).

%% 测试
tc(F, N) ->
  Time1 = os:timestamp(),
  do_times(N, F),
  Time2 = os:timestamp(),
  MicroDiffPerTime = timer:now_diff(Time2, Time1) / N,
  io:format("Times :~w ,  Each time consume: ~w us\n", [N, MicroDiffPerTime]).

do_times(N, F) when N =< 0 ->
  F();
do_times(N, F) ->
  F(),
  do_times(N - 1, F).

%% 遍历ets的所有key
ets_foreach_key(Fun, Table) ->
  ets:safe_fixtable(Table, true),
  First = ets:first(Table),
  try
    do_ets_foreach_key(Fun, First, Table)
  after
    ets:safe_fixtable(Table, false)
  end.

do_ets_foreach_key(F, Key, Table) ->
  case Key of
    '$end_of_table' ->
      ok;
    _ ->
      F(Key),
      do_ets_foreach_key(F, ets:next(Table, Key), Table)
  end.
%% 获取ets所有key
ets_all_key(Table) ->
  ets:safe_fixtable(Table, true),
  First = ets:first(Table),
  try
    do_ets_all_key(First, Table, [])
  after
    ets:safe_fixtable(Table, false)
  end.

do_ets_all_key(Key, Table, Result) ->
  case Key of
    '$end_of_table' ->
      Result;
    _ ->
      do_ets_all_key(ets:next(Table, Key), Table, [Key | Result])
  end.

catch_binary_to_term(Binary) ->
  if is_binary(Binary) ->
    case catch binary_to_term(Binary) of
      {'EXIT', _Reason} ->
        [];
      Term ->
        Term
    end;
    true ->
      lager:error("wrong use of function util:catch_binary_to_term,when arg is not binary..~w", [Binary]),
      []
  end.

%% 计算源码行数
count_src_line_num(Dir) ->
  count_src_line_num(Dir, [".+data_.+\.erl"]).
count_src_line_num(Dir, DisCardRegularExpList) ->
  count_src_line_num(Dir, DisCardRegularExpList, ".+\.erl$").
count_src_line_num(Dir, DisCardRegularExpList, MatchExp) ->
  {TotalNum, Info} =
    filelib:fold_files(Dir, MatchExp, true, fun(FileName, {LineAcc, InfoList}) ->
      case lists:any(fun(E) ->
        case re:run(FileName, E) of
          {match, _} ->
            true;
          _ ->
            false
        end
      end, DisCardRegularExpList) of
        true ->
          {LineAcc, InfoList};
        false ->
          ThisFileLineNum = cacl_file_line(FileName),
          %io:format("~1000p ------ ~w\n",[filename:basename(FileName), ThisFileLineNum]),
          {LineAcc + ThisFileLineNum, [{ThisFileLineNum, FileName} | InfoList]}
      end
    end, {0, []}),
  Info2 = lists:keysort(1, Info),
  lists:foreach(fun({Lines, Name}) ->
    io:format("~1000p ------ ~w\n", [filename:basename(Name), Lines])
  end, Info2),
  io:format("TotalNum=~w\n", [TotalNum]).


cacl_file_line(FileName) ->
  {ok, Bin} = file:read_file(FileName),
  cacl_file_line2(binary_to_list(Bin), 0, false).
cacl_file_line2([], Acc, true) ->
  Acc + 1;
cacl_file_line2([], Acc, false) ->
  Acc;
cacl_file_line2([$\n | Rest], Acc, true) ->
  cacl_file_line2(Rest, Acc + 1, false);
cacl_file_line2([$\n | Rest], Acc, false) ->
  cacl_file_line2(Rest, Acc, false);
cacl_file_line2([_E | Rest], Acc, true) ->
  cacl_file_line2(Rest, Acc, true);
cacl_file_line2([$\r | Rest], Acc, false) ->
  cacl_file_line2(Rest, Acc, false);
cacl_file_line2([$\  | Rest], Acc, false) ->
  cacl_file_line2(Rest, Acc, false);
cacl_file_line2([_ | Rest], Acc, false) ->
  cacl_file_line2(Rest, Acc, true).

%% @doc convert IP(tuple) to string()
ip_to_str(IP) ->
  case IP of
    {A, B, C, D} ->
      lists:concat([A, ".", B, ".", C, ".", D]);
    {A, B, C, D, E, F, G, H} ->
      lists:concat([A, ":", B, ":", C, ":", D, ":", E, ":", F, ":", G, ":", H]);
    Str when is_list(Str) ->
      Str;
    _ ->
      []
  end.

%% @doc 简单的mapreduce,仅供脚本使用
upmap(F, L) ->
  Parent = self(),
  Ref = make_ref(),
  [receive
    {Ref, Result} ->
      Result
  end || _ <- [spawn(fun() -> Parent ! {Ref, F(X)} end)
    || X <- L]].


gen_random_seed() ->
  Reg = ets:update_counter(?ets_random_seed, seed, 1),
  {A, B, C} = os:timestamp(),
  {A + Reg, B + Reg + 1, C + Reg + 2}.

latin1(Name) ->
  Name2 =
    if is_binary(Name) ->
      binary_to_list(Name);
      true ->
        Name
    end,
  Name3 =
    case is_name_en(Name2) of
      true ->
        Name2;
      false ->
        unicode:characters_to_binary(Name2)
    end,
  Name3.

is_name_en(Name) ->
  lists:all(fun(E) ->
    if E =< 255 ->
      true;
      true ->
        false
    end
  end, Name).

words_check(Words) ->
  WordsList = data_words:get(words_list),
  lists:all(fun(Word) ->
    case re:run(Words, Word, [caseless]) of
      {match, _} ->
        false;
      _ ->
        true
    end
  end, WordsList).

words_filter(Words_for_filter) ->
  Words_List = data_words:get(words_list),
  binary:bin_to_list(lists:foldl(fun(Kword, Words_for_filter0) ->
    re:replace(Words_for_filter0, Kword, "*", [global, caseless, {return, binary}])
  end, Words_for_filter, Words_List)).


check_dirty_words() ->
  Words_List = data_words:get(words_list),
  {B, _} = lists:foldl(fun(Kword, {Words_for_filter0, Num}) ->
    io:format("check ~w~n", [Num]),
    {re:replace(Words_for_filter0, Kword, "*", [global, caseless, {return, binary}]), Num + 1}
  end, {"11111111111", 1}, Words_List),
  binary:bin_to_list(B).

words_filter_2(Sentence0) ->
  Sentence = re:replace(Sentence0, "(\\b)space", "\\1Space", [global, caseless, {return, binary}]),
  re:replace(Sentence, "(\\b)end(\\b)", "\\1End\\2", [global, caseless, {return, binary}]).

check_is_mul({{_, _, _}, {_, _, _}} = StartDateTime, {{_, _, _}, {_, _, _}} = EndDateTime) ->
  DateTime = calendar:local_time(),
  StartDateTime =< DateTime andalso DateTime =< EndDateTime;
check_is_mul({StartOpenDayNum, {_, _, _} = StartTime}, {EndOpenDayNum, {_, _, _} = EndTime}) ->
  {OpenDate, _} = data_config:get_server_open_time(),
  StartTimestamp = util:datetime_to_seconds({OpenDate, StartTime}) + (StartOpenDayNum - 1) * ?one_day_seconds,
  StopTimestamp = util:datetime_to_seconds({OpenDate, EndTime}) + (EndOpenDayNum - 1) * ?one_day_seconds,
  Timestamp = util:datetime_to_seconds(erlang:localtime()),
  StartTimestamp =< Timestamp andalso Timestamp =< StopTimestamp;
check_is_mul(_, _) ->
  lager:error("错误的时间配置", []),
  false.

%% 输入的list为已经解码的utf8 list，检查字符串，只能为数字、大小写英文字母和汉字
check_blankName([]) ->
  true;
check_blankName([H | T]) ->
  case (H >= 48 andalso H =< 57) orelse (H >= 65 andalso H =< 90) orelse (H >= 97 andalso H =< 122) orelse (H >= 16#4e00 andalso H =< 16#9fa5) of
    false ->
      false;
    true ->
      check_blankName(T)
  end.

%% 输入的list为已经解码的utf8 list，计算长度，汉字长度为2
calc_name_length(DecodeList) ->
  lists:foldr(fun(Val, Acc) ->
    case Val >= 16#4e00 andalso Val =< 16#9fa5 of
      true ->
        Acc + 2;
      false ->
        Acc + 1
    end
  end, 0, DecodeList).
%%计算utf8 长度
cal_utf8_length(Utf8List) ->
  FunExtra = fun(Char, Acc) ->
    if
      Char =:= 2#11111100 ->
        Acc + 5;
      Char =:= 2#11111000 ->
        Acc + 4;
      Char =:= 2#11110000 ->
        Acc + 3;
      Char =:= 2#11100000 ->
        Acc + 2;
      Char =:= 2#11000000 ->
        Acc + 1;
      true ->
        Acc + 0
    end
  end,
  DecLength = lists:foldl(FunExtra, 0, Utf8List),
  length(Utf8List) - DecLength.


from_utf8(<<Data/binary>>) ->
  from_utf8(Data, [], []).
from_utf8(<<0:1, A:7, Rest/binary>>, Acc, Tail) ->
  %% 7 bits: 0yyyyyyy
  from_utf8(Rest, [A | Acc], Tail);
from_utf8(<<>>, Acc, Tail) ->
  lists:reverse(Acc, Tail);
from_utf8(<<6:3, A:5, 2:2, B:6, Rest/binary>>, Acc, Tail)
  when A >= 2 ->
  %% 11 bits: 110xxxxy 10yyyyyy
  from_utf8(Rest, [A * 64 + B | Acc], Tail);
from_utf8(<<14:4, A:4, 2:2, B:6, 2:2, C:6, Rest/binary>>, Acc, Tail)
  when A > 0; B >= 32 ->
  %% 16 bits: 1110xxxx 10xyyyyy 10yyyyyy
  Ch = (A * 64 + B) * 64 + C,
  if Ch < 16#D800; Ch > 16#DFFF, Ch < 16#FFFE ->
    from_utf8(Rest, [Ch | Acc], Tail)
  end;
from_utf8(<<30:5, A:3, 2:2, B:6, 2:2, C:6, 2:2, D:6, Rest/binary>>, Acc, Tail)
  when A > 0; B >= 16 ->
  %% 21 bits: 11110xxx 10xxyyyy 10yyyyyy 10yyyyyy
  from_utf8(Rest, [((A * 64 + B) * 64 + C) * 64 + D | Acc], Tail);
from_utf8(<<62:6, A:2, 2:2, B:6, 2:2, C:6, 2:2, D:6, 2:2, E:6, Rest/binary>>,
    Acc, Tail)
  when A > 0; B >= 8 ->
  %% 26 bits: 111110xx 10xxxyyy 10yyyyyy 10yyyyyy 10yyyyyy
  from_utf8(Rest, [(((A * 64 + B) * 64 + C) * 64 + D) * 64 + E | Acc], Tail);
from_utf8(<<126:7, A:1, 2:2, B:6, 2:2, C:6, 2:2, D:6, 2:2, E:6, 2:2, F:6,
  Rest/binary>>, Acc, Tail)
  when A > 0; B >= 4 ->
  %% 31 bits: 1111110x 10xxxxyy 10yyyyyy 10yyyyyy 10yyyyyy 10yyyyyy
  from_utf8(Rest, [((((A * 64 + B) * 64 + C) * 64 + D) * 64 + E) * 64 + F | Acc], Tail);
from_utf8(Bin, Acc, Tail) ->
  io:format("ucs Error: Bin=~p~n     Acc=~p~n     Tail=~p~n", [Bin, Acc, Tail]),
  {error, not_utf8}.

%% 将utf8编码的二进制数据解码成字符编码list
gen_utf8_decode_list(<<Bin/binary>>) ->
  gen_utf8_decode_list(Bin, []);
gen_utf8_decode_list(L) when is_list(L) ->
  gen_utf8_decode_list(list_to_binary(L)).
gen_utf8_decode_list(<<>>, AccDecodeList) ->
  lists:reverse(AccDecodeList);
gen_utf8_decode_list(<<0:1, X1:1, X2:1, X3:1, X4:1, X5:1, X6:1, X7:1,
  Left/binary>>, AccDecodeList) ->
  Val = get_val([X1, X2, X3, X4, X5, X6, X7]),
  gen_utf8_decode_list(Left, [Val | AccDecodeList]);
gen_utf8_decode_list(<<1:1, 1:1, 0:1, X1:1, X2:1, X3:1, X4:1, X5:1,
  1:1, 0:1, X6:1, X7:1, X8:1, X9:1, X10:1, X11:1,
  Left/binary>>, AccDecodeList) ->
  Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11]),
  gen_utf8_decode_list(Left, [Val | AccDecodeList]);
gen_utf8_decode_list(<<1:1, 1:1, 1:1, 0:1, X1:1, X2:1, X3:1, X4:1,
  1:1, 0:1, X5:1, X6:1, X7:1, X8:1, X9:1, X10:1,
  1:1, 0:1, X11:1, X12:1, X13:1, X14:1, X15:1, X16:1,
  Left/binary>>, AccDecodeList) ->
  Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]),
  gen_utf8_decode_list(Left, [Val | AccDecodeList]);
gen_utf8_decode_list(<<1:1, 1:1, 1:1, 1:1, 0:1, X1:1, X2:1, X3:1,
  1:1, 0:1, X4:1, X5:1, X6:1, X7:1, X8:1, X9:1,
  1:1, 0:1, X10:1, X11:1, X12:1, X13:1, X14:1, X15:1,
  1:1, 0:1, X16:1, X17:1, X18:1, X19:1, X20:1, X21:1,
  Left/binary>>, AccDecodeList) ->
  Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21]),
  gen_utf8_decode_list(Left, [Val | AccDecodeList]);
gen_utf8_decode_list(<<1:1, 1:1, 1:1, 1:1, 1:1, 0:1, X1:1, X2:1,
  1:1, 0:1, X3:1, X4:1, X5:1, X6:1, X7:1, X8:1,
  1:1, 0:1, X9:1, X10:1, X11:1, X12:1, X13:1, X14:1,
  1:1, 0:1, X15:1, X16:1, X17:1, X18:1, X19:1, X20:1,
  1:1, 0:1, X21:1, X22:1, X23:1, X24:1, X25:1, X26:1,
  Left/binary>>, AccDecodeList) ->
  Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26]),
  gen_utf8_decode_list(Left, [Val | AccDecodeList]);
gen_utf8_decode_list(<<1:1, 1:1, 1:1, 1:1, 1:1, 1:1, 0:1, X1:1,
  1:1, 0:1, X2:1, X3:1, X4:1, X5:1, X6:1, X7:1,
  1:1, 0:1, X8:1, X9:1, X10:1, X11:1, X12:1, X13:1,
  1:1, 0:1, X14:1, X15:1, X16:1, X17:1, X18:1, X19:1,
  1:1, 0:1, X20:1, X21:1, X22:1, X23:1, X24:1, X25:1,
  1:1, 0:1, X26:1, X27:1, X28:1, X29:1, X30:1, X31:1,
  Left/binary>>, AccDecodeList) ->
  Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31]),
  gen_utf8_decode_list(Left, [Val | AccDecodeList]);
gen_utf8_decode_list(Binary, AccDecodeList) ->
  lager:error("非法的utf8编码，Binary：~w, AccDecodeList：~w", [Binary, AccDecodeList]),
  [].

%% 生成单个字符的编码
get_val(List) ->
  {_, Val} =
    lists:foldr(fun(Elem, {Count, AccVal}) ->
      {Count + 1, AccVal + math:pow(2, Count) * Elem}
    end, {0, 0}, List),
  erlang:trunc(Val).

%% 在一个日期上增加一天
day_add({_, _, _} = Day, AddDay) ->
  calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Day) + AddDay);
day_add({Date, Time}, AddDay) ->
  {calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + AddDay), Time}.

%% 两个时间之间相关的秒
day_between_secs({{_, _, _}, {_, _, _}} = D1, {{_, _, _}, {_, _, _}} = D2) ->
  Secs = calendar:datetime_to_gregorian_seconds(D1) - calendar:datetime_to_gregorian_seconds(D2),
  abs(Secs).

%% 用分隔号把列表转化为字符串
-spec append_list(List :: [term()], Spliter :: atom()|string()) -> string().
append_list(List, Spliter) ->
  {L1, _} =
    lists:foldl(fun(E, {Acc, Count}) ->
      Acc2 =
        if
          Count == 0 -> Acc;
          true -> [Spliter | Acc]
        end,
      {[any_to_list(E) | Acc2], Count + 1}
    end, {[], 0}, lists:reverse(List)),
  lists:concat(L1).

any_to_list(B) when is_binary(B) -> erlang:binary_to_list(B);
any_to_list(B) when is_integer(B) -> erlang:integer_to_list(B);
any_to_list(B) -> B.

%% 数字转化为字符串，不足长度前面加0
integer_to_len_str(Integer, LenNeed) ->
  IStr = integer_to_list(Integer),
  Len2 = LenNeed - length(IStr),
  integer_to_len_str(IStr, Len2, "0").

integer_to_len_str(L2, 0, _Adder) ->
  L2;
integer_to_len_str(L2, Len, Adder) ->
  integer_to_len_str(Adder ++ L2, Len - 1, Adder).


hasValue(Value, Pos) ->
  erlang:trunc(math:pow(2, Pos)) band Value > 0.
addValue(Value, Pos) ->
  erlang:trunc(math:pow(2, Pos)) bor Value.
revValue(Value, Pos) ->
  erlang:trunc(math:pow(2, Pos)) band Value bxor Value.

get_reset_day(DateTime) ->
  {{_, _Month1, _} = Today, {_Hour, _, _}} = DateTime,
  Today.
%% 	if
%% 		Hour >= 4 ->%今天（凌晨4点后才算今天）
%% 			Today;
%% 		true ->%昨天
%% 			util:day_add(Today,-1)
%% 	end.

-define(random_str, "abcdefghijklmnopqrstuvwxyz0123456789").
random_str(N) ->
  rand:seed(os:timestamp()),
  Len = length(?random_str),
  [lists:nth(rand:uniform(Len), ?random_str) || _ <- lists:seq(1, N)].


list_replace(DelItem, NewItem, [DelItem | Rest]) -> [NewItem | Rest];
list_replace(DelItem, NewItem, [H | Rest]) ->
  [H | list_replace(DelItem, NewItem, Rest)];
list_replace(_, _, []) -> [].

list_replace_nth(N, NewItem, List) -> list_replace_nth(1, N, NewItem, List).
list_replace_nth(TheN, TheN, NewItem, [_DelItem | Rest]) -> [NewItem | Rest];
list_replace_nth(N, TheN, NewItem, [H | Rest]) ->
  [H | list_replace_nth(N + 1, TheN, NewItem, Rest)];
list_replace_nth(_, _, _, []) -> [].

gbtree_next(Key, {_, T}) ->
  gbtree_next(Key, T, nil, nil, nil).

gbtree_next(_, nil, Sentinel, Parent, Side) ->
  case Side of
    left ->
      Parent;
    right ->
      Sentinel;
    _ ->
      nil
  end;
gbtree_next(Key, {Key1, Value1, Smaller, Bigger}, Sentinel, Parent, Side) ->
  if
    Key < Key1 ->
      gbtree_next(Key, Smaller, {Key1, Value1}, {Key1, Value1}, left);
    Key > Key1 ->
      gbtree_next(Key, Bigger, Sentinel, {Key1, Value1}, right);
    true ->
      case Bigger of
        nil ->
          case Side of
            left ->
              Parent;
            right ->
              Sentinel;
            _ ->
              nil
          end;
        _ ->
          gb_trees:smallest(Bigger)
      end
  end.

gbtree_prev(Key, {_, T}) ->
  gbtree_prev(Key, T, nil, nil, nil).

gbtree_prev(_, nil, Sentinel, Parent, Side) ->
  case Side of
    left ->
      Sentinel;
    right ->
      Parent;
    _ ->
      nil
  end;
gbtree_prev(Key, {Key1, Value1, Smaller, Bigger}, Sentinel, Parent, Side) ->
  if
    Key < Key1 ->
      gbtree_prev(Key, Smaller, Sentinel, {Key1, Value1}, left);
    Key > Key1 ->
      gbtree_prev(Key, Bigger, {Key1, Value1}, {Key1, Value1}, right);
    true ->
      case Smaller of
        nil ->
          case Side of
            left ->
              Sentinel;
            right ->
              Parent;
            _ ->
              nil
          end;
        _ ->
          gb_trees:largest(Smaller)
      end
  end.

%% Stime开始时间，ETime结束时间，指定时间点如[{1,0,0},{2,0,0}]
cacl_reward_times(STime, ETime, Times) ->
  {D1, T1} = STime, %resettime
  {D2, T2} = ETime, %now
  P1 = calendar:date_to_gregorian_days(D2) - calendar:date_to_gregorian_days(D1) - 1,
  A1 = lists:foldl(fun(T, Sum) -> if T1 < T -> Sum + 1;true -> Sum end end, 0, Times),
  A2 = lists:foldl(fun(T, Sum) -> if T2 > T -> Sum + 1;true -> Sum end end, 0, Times),
  %% 	io:format("P1:~w,A1:~w,A2:~w~n",[P1,A1,A2]),
  P1 * length(Times) + A1 + A2.

datetime_shift(Datetime) ->
  Datetime.

datetime_unshift(Datetime) ->
  Datetime.

day_diff() ->
  day_diff({1970, 1, 1}).

day_diff(Seconds) when is_integer(Seconds) ->
  {LastDate, _} = util:seconds_to_datetime(Seconds),
  NowDate = erlang:date(),
  day_diff(LastDate, NowDate);
day_diff({Day, _}) ->
  day_diff(Day);

day_diff(Day) ->
  NowDate = erlang:date(),
  day_diff(Day, NowDate).

day_diff({Day1, _}, {Day2, _}) ->
  day_diff(Day1, Day2);
day_diff(Seconds1, Seconds2) when is_integer(Seconds1), is_integer(Seconds2) ->
  Day1 = util:seconds_to_datetime(Seconds1),
  Day2 = util:seconds_to_datetime(Seconds2),
  day_diff(Day1, Day2);
day_diff(Day1, Day2) ->
  erlang:abs(calendar:date_to_gregorian_days(Day1) - calendar:date_to_gregorian_days(Day2)).

source(Fname, Beam) ->
  {ok, F} = file:open(Fname, [write]),
  {ok, {_, [{abstract_code, {_, Ac}}]}} = beam_lib:chunks(code:which(Beam), [abstract_code]),
  file:write(F, io_lib:format("~s", [erl_prettypr:format(erl_syntax:form_list(Ac))])),
  file:close(F).


%%====================玩家进程内部 事物接口   START===============================
-define(other_backup_list, other_backup_list).
transaction(Fun) ->
  Bak = transaction_dobackup(),
  erlang:put(?other_backup_list, []),

  case ?CATCH(Fun()) of
    {'EXIT', ErrTypeReason} ->
      transaction_rollback_other(),
      transaction_rollback(Bak),
      {aborted, ErrTypeReason};
    Ret ->
      on_transaction_commit_other(),
      {atomic, Ret}
  end.

transaction_dobackup() ->
  RoleInfo = role_data:get_roleInfo(),
  GerAllInfo = role_data:get_gerAll(),
  BagEquip = role_data:get_bagEquip(),
  BagItem = role_data:get_bagItem(),
  {RoleInfo, GerAllInfo, BagEquip, BagItem}.

transaction_rollback(Bak) ->
  {RoleInfo, GerAllInfo, BagEquip, BagItem} = Bak,
  role_data:set_roleInfo(RoleInfo),
  role_data:set_gerAll(GerAllInfo),
  role_data:set_bagEquip(BagEquip),
  role_data:set_bagItem(BagItem).

transaction_backup_other(Key, Value) ->
  List = erlang:get(?other_backup_list),
  case lists:keyfind(Key, 1, List) of
    false ->
      List2 = [{Key, Value} | List],
      erlang:put(?other_backup_list, List2);
    _ ->
      throw(already_backup)
  end,
  ok.


transaction_rollback_other() ->
  List = erlang:get(?other_backup_list),
  transaction_rollback_other2(List).

transaction_rollback_other2([]) ->
  ok;
transaction_rollback_other2([{Key, Value} | List]) ->
  erlang:put(Key, Value),
  transaction_rollback_other2(List).

on_transaction_commit_other() ->
  erlang:erase(?other_backup_list).

%%输入是 utf8 编码
%%过滤掉 emoji
filter_emoji(Input) ->
  %%utf8 最多3个字节，超过3个字节的一律过滤掉
  InputDecode = util:gen_utf8_decode_list(Input),
  FilteredCode =
    [
      Code ||
      Code <- InputDecode,
      Code < 16#FFFF andalso Code > 0
    ],
  case FilteredCode =/= InputDecode of
    true ->
      erlang:binary_to_list(unicode:characters_to_binary(FilteredCode));
    _ ->
      Input
  end.

%%浮点数精度检查
greater(A, B) ->
  A - B =< 0.00005.

lesss(A, B) ->
  A - B < -0.00005.

%%取绝对值
jue_dui(A) ->
  case A < 0 of
    true ->
      -A;
    false ->
      A
  end.

%%列表补0
loop_bu_zero(List, Num) ->
  Size = erlang:length(List),
  case Size >= Num of
    true ->
      List;
    false ->
      List ++ lists:duplicate(Num - Size, 0)
  end.

%%util_call，call超时后被访问的进程会继续执行该消息，util_call会取消超时时间
util_call(Pid, Msg) ->
  gen_server:call(Pid, {util_call, util:ms(), Msg}, ?util_call_timeout).

util_handle_call(Msg) ->
  case Msg of
    {util_call, LastMs, OriginalMsg} ->
      case util:ms() - LastMs > ?util_call_timeout of
        true -> %%消息已经超时
          ?util_call_miss;
        _ ->
          OriginalMsg
      end;
    _ ->
      Msg
  end.

%%检查是否经过重置时间
check_reset_time(LastResetTick, LittleResetTime) ->
  NowSec = util:now(),
  {Date, _} = util:seconds_to_datetime(NowSec),
  {OldDate, _} = util:seconds_to_datetime(LastResetTick),
  ResetTime = util:datetime_to_seconds({Date, LittleResetTime}),
  case Date =:= OldDate of
    true ->
      NowSec >= ResetTime andalso LastResetTick =< ResetTime;
    false ->
      case util:day_add(OldDate, 1) == Date of
        true ->
          NowSec >= ResetTime;
        false ->
          true
      end
  end.

%%选择列表中的其中一个元素
choose_one_from_list(List, Limit) ->
  ResList = choose_one_from_limit(List, Limit),
  ResValue =
    case ResList of
      ?undefined ->
        {_, ResV} = lists:last(List),
        ResV;
      _ ->
        ResList
    end,
  case is_list(ResValue) of
    true ->
      util:random_one_from_list(ResValue);
    false ->
      ResValue
  end.


choose_one_from_limit(List, Limit) ->
  util:foldl(fun({K, V}, Acc) ->
    case Limit =< K of
      true ->
        {return, V};
      false ->
        Acc
    end
  end, ?undefined, List).


choose_reward_from_list(List, Limit) ->
  ResList = choose_one_from_limit(List, Limit),
  case ResList of
    ?undefined ->
      {_, ResV} = lists:last(List),
      ResV;
    _ ->
      ResList
  end.

%% 检查今天是不是周几
check_day_of_week(WeekDay) ->
  {Date, _Time} = calendar:universal_time(),
  case calendar:day_of_the_week(Date) of
    WeekDay ->
      true;
    _ ->
      false
  end.

nth_week() ->
  nth_week(timer_wheel:nowsec()).
nth_week(Time) ->
  {{Y1, M1, D1}, _} = seconds_to_datetime(Time),
  calendar:iso_week_number({Y1, M1, D1}).

is_same_week(0) -> false;
is_same_week(Time) ->
  is_same_week(Time, util:now()).

is_same_week(Time1, Time2) ->
  nth_week(Time1) == nth_week(Time2).


%% 检查今天是不是几号
check_day_of_month(LimitDay) ->
  {{_Year, _Month, Day}, _} = calendar:universal_time(),
  Day =:= LimitDay.

max_list(List) ->
  lists:foldl(fun(N, Acc) ->
    max(N, Acc)
  end, ?integer_min_value, List).

min_list(List) ->
  lists:foldl(fun(N, Acc) ->
    min(N, Acc)
  end, ?integer_max_value, List).

ip_ntoa(IP) ->
  IP0 = if is_tuple(IP) ->
    inet_parse:ntoa(IP);
    true ->
      IP
  end,
  case IP0 of
    {error, einval} ->
      lager:error("bad ip:~w", [IP]),
      "127.0.0.0";
    _ ->
      IP0
  end.

delSame([], Acc) -> Acc;
delSame([Pre | T], Acc) -> delSame([X || X <- T, X =/= Pre], Acc ++ [Pre]).

http_build_query(QueryString) ->
  string:strip(lists:foldl(
    fun({K, V}, Acc) ->
      Acc ++ K ++ "=" ++ V ++ "&"
    end, "", QueryString), both, $&).

if3(Cond, E1, E2) ->
  case (Cond) of true -> (case is_function(E1) of true -> apply(E1, []); false -> E1 end); false ->
    (case is_function(E2) of true -> apply(E2, []); false -> E2 end) end.

get_ip(Socket) ->
  case inet:peername(Socket) of
    {ok, {IP, _Port}} ->
      ip_ntoa(IP);
    _ ->
      ""
  end.

%%gc自己的进程
gc_self_work() ->
  timer_wheel:add_plan(util:now() + data_common:get(gc_frequency), {?MODULE, gc_self_work_2, []}).
gc_self_work_random() ->
  timer_wheel:add_plan(util:now() + util:random_int(1, data_common:get(gc_frequency)), {?MODULE, gc_self_work_2, []}).
gc_self_work_2() ->
  timer_wheel:add_plan(util:now() + data_common:get(gc_frequency), {?MODULE, gc_self_work_2, []}),
  case data_common:get(is_gc) of
    true ->
      erlang:garbage_collect(self());
    false ->
      ignore
  end.