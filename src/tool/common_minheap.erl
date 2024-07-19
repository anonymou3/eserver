%% @author wanbiyan
%% @doc 最小堆


-module(common_minheap).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/4,
  new/5,
  insert/2,
  delete_min/1,
  delete_with_key/2,
  clear/1,
  erase/1,
  sort/1,
  size/1,
  min/1,
  get/2]).


%% 创建新堆
%% HeapName: 堆名
%% MaxSize:  堆容量
%% BigThan:  比较函数,Data1 > Data2 为 true
%% KeyPos:   element(KeyPos,Data1) =:= element(KeyPos,Data2)
%%           则认为是同一条数据
-spec new(HeapName, MaxSize, BigThan, KeyPos) -> boolean() when
  HeapName :: atom(),
  MaxSize :: integer(),
  BigThan :: fun((Data1 :: T, Data :: T) -> boolean()),
  KeyPos :: integer(),
  T :: tuple().
new(HeapName, MaxSize, BigThan, KeyPos) ->
  new(HeapName, MaxSize, BigThan, KeyPos, []).

%% 创建新堆
%% HeapName: 堆名
%% MaxSize:  堆容量
%% BigThan:  比较函数,Data1 > Data2 为 true
%% KeyPos:   element(KeyPos,Data1) =:= element(KeyPos,Data2)
%%           则认为是同一条数据
%% DataList: 初始化数据
-spec new(HeapName, MaxSize, BigThan, KeyPos, Data) -> boolean() when
  HeapName :: atom(),
  MaxSize :: integer(),
  BigThan :: fun((Data1 :: T, Data :: T) -> boolean()),
  KeyPos :: integer(),
  Data :: [T] | map(),
  T :: tuple().
new(HeapName, MaxSize, BigThan, KeyPos, DataList) when is_list(DataList) ->
  case erlang:get(HeapName) of
    undefined ->
      case erlang:length(DataList) > MaxSize of
        false ->
          Heap = new(BigThan, DataList),
          erlang:put(HeapName, {MaxSize, BigThan, KeyPos, Heap}),
          true;
        _ ->
          false
      end;
    _ ->
      false
  end;
new(HeapName, MaxSize, BigThan, KeyPos, DataHeap) when is_map(DataHeap) ->
  case erlang:get(HeapName) of
    undefined ->
      erlang:put(HeapName, {MaxSize, BigThan, KeyPos, DataHeap}),
      true;
    _ ->
      false
  end.

%% 插入/更新数据
%% HeapName: 堆名
%% Data:数据
-spec insert(HeapName, Data :: T) -> boolean()|{true, OutData :: T} when
  HeapName :: atom(),
  T :: tuple().
insert(HeapName, Data) ->
  case erlang:get(HeapName) of
    undefined ->
      false;
    {MaxSize, BigThan, KeyPos, Heap} ->
      case is_duplicate(Data, KeyPos, Heap) of
        {true, Pos} ->
          NewHeap = replace(Pos, Data, BigThan, Heap),
          Result = true;
        false ->
          case insert(Data, MaxSize, BigThan, Heap) of
            {_, OutData, NewHeap} when is_tuple(OutData) ->
              Result = {true, element(KeyPos, OutData)};
            {Result, _, NewHeap} ->
              ok
          end
      end,
      erlang:put(HeapName, {MaxSize, BigThan, KeyPos, NewHeap}),
      Result
  end.

%% 删除堆顶数据
%% HeapName: 堆名
-spec delete_min(HeapName) -> boolean() when
  HeapName :: atom().
delete_min(HeapName) ->
  case erlang:get(HeapName) of
    undefined ->
      false;
    {MaxSize, BigThan, KeyPos, Heap} ->
      NewHeap = delete_min(BigThan, Heap),
      erlang:put(HeapName, {MaxSize, BigThan, KeyPos, NewHeap}),
      true
  end.

%% 删除指定Key的数据
%% HeapName: 堆名
%% Key:堆初始化时,指定的KeyPos对应的值
-spec delete_with_key(HeapName, Key) -> boolean() when
  HeapName :: atom(),
  Key :: term().
delete_with_key(HeapName, Key) ->
  case erlang:get(HeapName) of
    undefined ->
      false;
    {MaxSize, BigThan, KeyPos, Heap} ->
      NewHeap = delete_with_key(BigThan, Heap, KeyPos, Key),
      erlang:put(HeapName, {MaxSize, BigThan, KeyPos, NewHeap}),
      true
  end.

%% 清空堆
%% HeapName: 堆名
-spec clear(HeapName) -> boolean() when
  HeapName :: atom().
clear(HeapName) ->
  case erlang:get(HeapName) of
    undefined ->
      false;
    {MaxSize, BigThan, KeyPos, _Heap} ->
      erlang:put(HeapName, {MaxSize, BigThan, KeyPos, maps:new()}),
      true
  end.

%% 删除堆
%% HeapName: 堆名
-spec erase(HeapName) -> undefined | [T] when
  HeapName :: atom(),
  T :: map().
erase(HeapName) ->
  case erlang:erase(HeapName) of
    undefined ->
      undefined;
    {_, _, _, Heap} ->
      Heap
  end.

%% 排序
%% HeapName: 堆名
-spec sort(HeapName) -> false | [T] when
  HeapName :: atom(),
  T :: tuple().
sort(HeapName) ->
  case erlang:get(HeapName) of
    undefined ->
      false;
    {_MaxSize, BigThan, _KeyPos, Heap} ->
      sort(BigThan, Heap, [])
  end.

%% 获取堆大小
%% HeapName: 堆名
-spec size(HeapName) -> false | non_neg_integer() when
  HeapName :: atom().
size(HeapName) ->
  case erlang:get(HeapName) of
    undefined ->
      false;
    {_MaxSize, _BigThan, _KeyPos, Heap} ->
      maps:size(Heap)
  end.

%% 获取堆大小
%% HeapName: 堆名
%% 返回false:堆不存在
%% 返回undefined:堆顶不存在
-spec min(HeapName) -> false | undefined | T when
  HeapName :: atom(),
  T :: tuple().
min(HeapName) ->
  case erlang:get(HeapName) of
    undefined ->
      false;
    {_MaxSize, _BigThan, _KeyPos, Heap} ->
      maps:get(1, Heap, undefined)
  end.

%% 获取某一个值(少用)
-spec get(HeapName, Key) -> undefined | T when
  HeapName :: atom(),
  Key :: any(),
  T :: tuple().
get(HeapName, Key) ->
  case erlang:get(HeapName) of
    undefined ->
      undefined;
    {_MaxSize, _BigThan, KeyPos, Heap} ->
      case maps:fold(
        fun(_Key, Data, Acc) ->
          case erlang:element(KeyPos, Data) =:= Key of
            true ->
              Data;
            _ ->
              Acc
          end
        end, undefined, Heap) of
        Data when is_tuple(Data) ->
          Data;
        _ ->
          undefined
      end
  end.

%% ====================================================================
%% Internal functions
%% ====================================================================

new(BigThan, DataList) ->
  {Size, KvList} = lists:foldl(fun(Data, {Index, Acc}) ->
    {Index + 1, [{Index + 1, Data} | Acc]}
                               end, {0, []}, DataList),
  Map = maps:from_list(KvList),
  %% 从最后一个非叶子节点开始向上调整
  filterUp_all(Size, util:floor(Size / 2), BigThan, Map).

is_duplicate(Data, KeyPos, Map) ->
  case maps:keys(maps:filter(fun(_, DataTmp) ->
    erlang:element(KeyPos, DataTmp) =:= erlang:element(KeyPos, Data)
                             end, Map)) of
    [] ->
      false;
    [Pos] ->
      {true, Pos}
  end.

filterDown(BigThan, Map) ->
  Size = maps:size(Map),
  filterDown(Size, 1, BigThan, Map).
filterDown(0, _Size, _BigThan, Map) ->
  Map;
filterDown(Size, Size, _BigThan, Map) ->
  Map;
filterDown(Size, Index, BigThan, Map) ->
  IndexL = 2 * Index,
  case IndexL =< Size of
    true ->
      Top = maps:get(Index, Map),
      Left = maps:get(IndexL, Map),
      IndexR = 2 * Index + 1,
      case IndexR =< Size of
        true ->
          Right = maps:get(IndexR, Map),
          case BigThan(Top, Right) orelse BigThan(Top, Left) of
            true ->
              case BigThan(Right, Left) of
                true ->
                  Map0 = maps:update(Index, Left, Map),
                  Map1 = maps:update(IndexL, Top, Map0);
                false ->
                  Map0 = maps:update(Index, Right, Map),
                  Map1 = maps:update(IndexR, Top, Map0)
              end,
              Map2 = filterDown(Size, IndexL, BigThan, Map1),
              filterDown(Size, IndexR, BigThan, Map2);
            false ->
              Map2 = filterDown(Size, IndexL, BigThan, Map),
              filterDown(Size, IndexR, BigThan, Map2)
          end;
        false ->
          case BigThan(Top, Left) of
            true ->
              Map0 = maps:update(Index, Left, Map),
              maps:update(IndexL, Top, Map0);
            false ->
              Map
          end
      end;
    false ->
      Map
  end.

%% filterUp_all(BigThan,Map) ->
%% 	Size = maps:size(Map),
%% 	filterUp_all(Size,util:floor(Size/2),BigThan,Map).
filterUp_all(_Size, 0, _BigThan, Map) ->
  Map;
filterUp_all(Size, Index, BigThan, Map) ->
  Top = maps:get(Index, Map),
  IndexL = 2 * Index,
  IndexR = 2 * Index + 1,
  Left = maps:get(IndexL, Map),
  case IndexR > Size of
    true ->
      case BigThan(Top, Left) of
        true ->
          Map0 = maps:update(Index, Left, Map),
          Map1 = maps:update(IndexL, Top, Map0),
          filterUp_all(Size, Index - 1, BigThan, Map1);
        false ->
          filterUp_all(Size, Index - 1, BigThan, Map)
      end;
    false ->
      Right = maps:get(IndexR, Map),
      case BigThan(Top, Right) orelse BigThan(Top, Left) of
        true ->
          case BigThan(Right, Left) of
            true ->
              Map0 = maps:update(Index, Left, Map),
              Map1 = maps:update(IndexL, Top, Map0),
              NewMap = filterDown(Size, IndexL, BigThan, Map1),
              filterUp_all(Size, Index - 1, BigThan, NewMap);
            false ->
              Map0 = maps:update(Index, Right, Map),
              Map1 = maps:update(IndexR, Top, Map0),
              NewMap = filterDown(Size, IndexR, BigThan, Map1),
              filterUp_all(Size, Index - 1, BigThan, NewMap)
          end;
        false ->
          filterUp_all(Size, Index - 1, BigThan, Map)
      end
  end.

filterUp(1, BigThan, Map) ->
  filterDown(BigThan, Map);
filterUp(Index, BigThan, Map) ->
  case Index / 2 of
    IndexT when is_integer(IndexT) ->
      ok;
    _ ->
      IndexT = util:floor(Index / 2)
  end,
  Data = maps:get(Index, Map),
  Top = maps:get(IndexT, Map),
  case BigThan(Top, Data) of
    true ->
      Map0 = maps:update(Index, Top, Map),
      Map1 = maps:update(IndexT, Data, Map0),
      filterUp(IndexT, BigThan, Map1);
    false ->
      Size = maps:size(Map),
      filterDown(Size, Index, BigThan, Map)
  end.

replace(Pos, Data, BigThan, Map) ->
  Map0 = maps:put(Pos, Data, Map),
  filterUp(Pos, BigThan, Map0).

insert(Data, MaxSize, BigThan, Map) ->
  Size = maps:size(Map),
  case Size >= MaxSize of
    true ->
      Top = maps:get(1, Map),
      case BigThan(Data, Top) of
        true ->
          {true, Top, replace(1, Data, BigThan, Map)};
        false ->
          {false, undefined, Map}
      end;
    false ->
      Map0 = maps:put(Size + 1, Data, Map),
      NewSize = Size + 1,
      {true, undefined, filterUp(NewSize, BigThan, Map0)}
  end.

delete_min(BigThan, Map) ->
  Size = maps:size(Map),
  case Size > 0 of
    true ->
      Last = maps:get(Size, Map),
      Map0 = maps:update(1, Last, Map),
      Map1 = maps:remove(Size, Map0),
      filterDown(BigThan, Map1);
    false ->
      Map
  end.

delete_with_key(BigThan, Map, KeyPos, Key) ->
  TmpMap = maps:filter(fun(_, Data) -> erlang:element(KeyPos, Data) =:= Key end, Map),
  case maps:keys(TmpMap) of
    [] ->
      Map;
    [Pos | _] ->
      Size = maps:size(Map),
      case Size =:= Pos of
        true ->
          maps:remove(Pos, Map);
        false ->
          Last = maps:get(Size, Map),
          Map0 = maps:update(Pos, Last, Map),
          Map1 = maps:remove(Size, Map0),
          filterUp(Pos, BigThan, Map1)
      end
  end.


sort(BigThan, Map, List) ->
  Size = maps:size(Map),
  case Size =:= 0 of
    true ->
      List;
    false ->
      Top = maps:get(1, Map),
      sort(BigThan, delete_min(BigThan, Map), [Top | List])
  end.
