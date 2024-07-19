%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(random2).

%% Reasonable random number generator.
%%  The method is attributed to B. A. Wichmann and I. D. Hill
%%  See "An efficient and portable pseudo-random number generator",
%%  Journal of Applied Statistics. AS183. 1982. Also Byte March 1987.

-export([seed/0, seed/1, seed/3, uniform/0, uniform/1,
  uniform_s/1, uniform_s/2, seed0/0, init/0]).

-export([select_list/2, random_int/2, random_float/2]).

-compile(export_all).
-define(PRIME1, 30269).
-define(PRIME2, 30307).
-define(PRIME3, 30323).

%%-----------------------------------------------------------------------
%% The type of the state

-type ran() :: {integer(), integer(), integer()}.

%%-----------------------------------------------------------------------

-spec select_list(integer(), list()) -> list().


%% @doc 从[Lower...Higher]包括边界的整数区间中随机一个数
random_int(Lower, Lower) ->
  Lower;
random_int(Lower, Higher) when Lower < Higher ->
  uniform(Higher - Lower + 1) + Lower - 1;
random_int(Higher, Lower) ->
  random_int(Lower, Higher).

%% @doc 从(lower...Higher)中随机出一个浮点数出来
random_float(Lower, Lower) ->
  Lower;
random_float(Lower, Higher) ->
  uniform() * (Higher - Lower) + Lower.

%% @doc 从一个列表中随机抽取N个，顺序随机 ，N可以超过界限
select_list(N, List) ->
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
      Rand = uniform(Length),
      {value, Select, Rest} = util:nth_take(Rand, List),
      random_list2(Rest, N - 1, Length - 1, [Select | Result])
  end.

-spec init() -> ok.

init() ->
  catch ets:new(random_seed, [named_table, set, public, {keypos, 1}]),
  ok.

-spec seed0() -> ran().

seed0() ->
  {3172, 9814, 20125}.

%% seed()
%%  Seed random number generation with default values

-spec seed() -> ran().

seed() ->
  case seed_put(seed0()) of
    undefined -> seed0();
    {_, _, _} = Tuple -> Tuple
  end.


%% seed({A1, A2, A3}) 
%%  Seed random number generation 

-spec seed({A1, A2, A3}) -> 'undefined' | ran() when
  A1 :: integer(),
  A2 :: integer(),
  A3 :: integer().

seed({A1, A2, A3}) ->
  seed(A1, A2, A3).

%% seed(A1, A2, A3) 
%%  Seed random number generation 

-spec seed(A1, A2, A3) -> 'undefined' | ran() when
  A1 :: integer(),
  A2 :: integer(),
  A3 :: integer().

seed(A1, A2, A3) ->
  seed_put({(abs(A1) rem (?PRIME1 - 1)) + 1,   % Avoid seed numbers that are
    (abs(A2) rem (?PRIME2 - 1)) + 1,   % even divisors of the
    (abs(A3) rem (?PRIME3 - 1)) + 1}). % corresponding primes.


-spec seed_put(ran()) -> 'undefined' | ran().

seed_put(Seed) ->
  ets:insert(random_seed, {self(), Seed}).

-spec seed_get() -> 'undefined' | ran().

seed_get() ->
  case ets:lookup(random_seed, erlang:self()) of
    [{_, Seed}] ->
      Seed;
    _ ->
      undefined
  end.

%% uniform()
%%  Returns a random float between 0 and 1.

-spec uniform() -> float().

uniform() ->
  {A1, A2, A3} = case seed_get() of
                   undefined -> seed0();
                   Tuple -> Tuple
                 end,
  B1 = (A1 * 171) rem ?PRIME1,
  B2 = (A2 * 172) rem ?PRIME2,
  B3 = (A3 * 170) rem ?PRIME3,
  seed_put({B1, B2, B3}),
  R = B1 / ?PRIME1 + B2 / ?PRIME2 + B3 / ?PRIME3,
  R - trunc(R).

%% uniform(N) -> I
%%  Given an integer N >= 1, uniform(N) returns a random integer
%%  between 1 and N.

-spec uniform(N) -> pos_integer() when
  N :: pos_integer().

uniform(N) when is_integer(N), N >= 1 ->
  trunc(uniform() * N) + 1.


%%% Functional versions

%% uniform_s(State) -> {F, NewState}
%%  Returns a random float between 0 and 1.

-spec uniform_s(State0) -> {float(), State1} when
  State0 :: ran(),
  State1 :: ran().

uniform_s({A1, A2, A3}) ->
  B1 = (A1 * 171) rem ?PRIME1,
  B2 = (A2 * 172) rem ?PRIME2,
  B3 = (A3 * 170) rem ?PRIME3,
  R = B1 / ?PRIME1 + B2 / ?PRIME2 + B3 / ?PRIME3,
  {R - trunc(R), {B1, B2, B3}}.

%% uniform_s(N, State) -> {I, NewState}
%%  Given an integer N >= 1, uniform(N) returns a random integer
%%  between 1 and N.

-spec uniform_s(N, State0) -> {integer(), State1} when
  N :: pos_integer(),
  State0 :: ran(),
  State1 :: ran().

uniform_s(N, State0) when is_integer(N), N >= 1 ->
  {F, State1} = uniform_s(State0),
  {trunc(F * N) + 1, State1}.

test() ->
  tc:run(100000, fun() -> uniform() end),
  tc:run(100000, fun() -> select_list(3, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]) end).

test2() ->
  tc:run(100000, fun() -> rand:uniform() end),
  tc:run(100000, fun() -> util:random_list2([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], 3) end).
