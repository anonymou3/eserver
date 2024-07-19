%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 3月 2023 10:41
%%%-------------------------------------------------------------------
-author("new").

-ifndef(record_hrl).
-define(record_hrl, 1).
%% 玩家基础属性结构

-include("common.hrl").

%%全局数据 list类型
-record(r_public_data, {
  subKey,
  val
}).

%%master中心服公共数据服务common结构体
-record(master_common, {
  roleID = 0,
  roleName = "",
  serverID = 0
}).

-record(role, {
  roleID = 0                  %% 全局唯一编号
  , accID = 0                    %% 账号
  , accName = ""                %% 玩家在平台的账号
  , srctype = 0                    %% 平台类型
  , appID = 0                       %% 平台ID
  , sid = 0                      %% 创建角色的服务器编号
  , roleName = ""                %% 角色名
  , sex = 0              %% 性别1男2女
  , level = 0                    %% 等级
  , exp = 0                      %% 当前经验
}).

%%玩家杂项
-record(role_misc, {
  roleID = 0                          %% 全局唯一编号
  , dictData = dict:new()             %% 杂项
}).

-endif.
