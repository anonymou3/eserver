%% @doc 服务器所有ID生成相关接口
%% 请确保合服时不用修改ID

-module(tk_id).
-include("common.hrl").
-compile(export_all).
%% API functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
init() ->
  init_id(roleID, "roleID", "g_role", ?ROLE_ID_BASE),
  init_id(familyID, " unionID", "g_union", ?FAMILY_ID_BASE),
  ItemID1 = init_id2("itemUID", "g_equip", ?ITEM_ID_BASE),
  ItemID2 = init_id2("itemUID", "g_bag_item", ?ITEM_ID_BASE),
  ItemID3 = init_id2("gerID", "g_ger", ?GER_ID_BASE),
  ItemID4 = erlang:max(erlang:max(ItemID1, ItemID2), ItemID3),
  ets:insert(?ets_id, {itemUID, ItemID4}).

init_id(IDName, Key, Table, Base) ->
  ID = init_id2(Key, Table, Base),
  ets:insert(?ets_id, {IDName, ID}).

init_id2(Key, Table, Base) ->
  Sql = io_lib:format("select max(~s) from ~s;", [Key, Table]),
  case db_sql:get_row(Sql) of
    [Max] when is_integer(Max) ->
      next;
    _ ->
      Max = 0
  end,
  erlang:max(Max, Base).

init_id3(Key, Base) ->
  Sql = io_lib:format("SELECT uidValue from g_uid where startUid=~w", [Key]),
  case db_sql:get_row(Sql) of
    [Max] when is_integer(Max) ->
      ok;
    _ ->
      Max = 0
  end,
  erlang:max(Max + 10000, Base).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc 生成玩家ID,请注意，务必大于10000，否则会与pvp中的rank重复
gen_roleID() ->
  ets:update_counter(?ets_id, roleID, 1).

%% @doc 生成武将iD
gen_gerID() ->
  ets:update_counter(?ets_id, itemUID, 1).

%% @doc 生成联盟ID
gen_familyID() ->
  ets:update_counter(?ets_id, familyID, 1).

gen_unionAuctionID() ->
  ets:update_counter(?ets_id, unionAuctionID, 1).

%%拍卖物品ID
gen_auctionID() ->
  ets:update_counter(?ets_id, auctionID, 1).


%% 邀请码生成规则
%% 基础定义：RoleID=玩家ID，
%% 	  "0"=0
%% 	  "1"=1
%% 	     .
%% 	     .
%% 	     .
%% 	  "9"=9
%% 	  "a"=10
%% 	  "b"=11
%% 	  "c"=12
%% 	  "d"=13
%% 	     .
%% 	     .
%% 	     .
%% 	  "z"=35
%%
%% RoleID生成邀请码：
%% 	1、RoleID依据基础定义中的规则，转化为36位的字符串Str1
%% 	2、将Str1逆序，得到Str2
%% 	3、如果Str2的长度小于6个字符，则在末尾依次补足6位，得到6位邀请码字符串Str3。补位规则：加z
%%
%% 邀请码还原成RoleID的规则：
%% 	1、将邀请码字符串Str3逆序得到Str4
%% 	2、如果第一位为z，则去掉第一位。否则直接进入下一步。
%% 	3、将步骤2获得的字符串，转化成36进制的数字，得到roleID
%% @doc 玩家ID生成邀请码
roleID2inviteCode(RoleID) ->
  Str1 = string:to_lower(integer_to_list(RoleID, 36)),
  Length = length(Str1),
  if Length > 6 ->
    exit("fatal error, roleID exceed planed");
    Length == 6 ->
      Str2 = lists:reverse(Str1),
      if hd(Str2) == $z ->
        exit("fatal error, roleID exceed planed");
        true ->
          Str2
      end;
    true ->
      lists:reverse(lists:duplicate(6 - Length, $z) ++ Str1)
  end.


%% @doc 邀请码转化成玩家ID
inviteCode2roleID(InviteCode) ->
  Str1 = lists:reverse(InviteCode),
  if hd(Str1) =:= $z ->
    Str2 = tl(Str1);
    true ->
      Str2 = Str1
  end,
  list_to_integer(Str2, 36).

%% base36_to_10(Base36) when Base36 >= $a -> Base36-$a+10;
%% base36_to_10(Base36) -> Base36-$0.
