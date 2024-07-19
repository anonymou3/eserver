%%%% @author
%%%% @doc test edoc.
%%%% created 2013-2-18
-module(util_master).
%%
%%-export([
%%  master_send/2,
%%  master_call/2,
%%  is_master/0,
%%  normal_send/3,
%%  center_send/2,
%%  center_call/2,
%%  get_self_server_id/0,
%%  get_server_name/0,
%%  get_all_war_server_id/0,
%%  war_zone_send/2,
%%  is_center/0,
%%  normal_call/3,
%%  all_normal_send/2,
%%  normal_role_send/3,
%%  send_role_msg/3,
%%  pid/2
%%]).
%%
%%-include("common.hrl").
%%-include("def_role.hrl").
%%
%%%%跨服需要用到的部分方法
%%master_send(M,Msg) ->
%%  erlang:send({M,node_server:get_master_node_in_dict()},Msg).
%%
%%master_call(M,Msg) ->
%%  gen_server:call({M,node_server:get_master_node_in_dict()},Msg,2000).
%%
%%%%给所有节点发送
%%all_normal_send(M,Msg) ->
%%  %%只能master调用,避免死循环
%%  case is_master() of
%%    true ->
%%      ets:foldl(fun(#r_node{} = RNode,Acc) ->
%%        case RNode#r_node.serverType of
%%          normal ->
%%            erlang:send({M,RNode#r_node.serverNode},Msg),
%%            Acc;
%%          _ ->
%%            Acc
%%        end
%%                end,true,?ETS_NODE);
%%    false ->
%%      ?ERR("all_normal_send use must by master node!!!"),
%%      ignore
%%  end.
%%
%%%%判断当前节点是不是主节点
%%is_master() ->
%%  data_setting:get(server_type) =:= master.
%%
%%%%给普通节点发送消息
%%normal_send(?undefined,_M,_Msg) ->
%%  ignore;
%%normal_send(ServerID,M,Msg) ->
%%  node_server:send_msg_to_node(M,ServerID,Msg).
%%
%%%%跨服发给玩家
%%normal_role_send(ServerID,RoleID,Msg) when is_integer(RoleID) ->
%%    catch node_server:send_msg_to_node(war_zone_server,ServerID,{master_role_send,RoleID,Msg}).
%%
%%%%发送给中心节点
%%center_send(M,Msg) ->
%%  case ets:lookup(?ETS_SHARE_DATA,center_node) of
%%    [] ->
%%      case util_master:is_master() of
%%        false ->
%%          throw(no_center);
%%        true ->
%%          ignore
%%      end;
%%    [#r_share_data{value = CenterNode}] ->
%%        catch erlang:send({M,CenterNode},Msg)
%%  end.
%%
%%%%发送结果给所有跨服节点
%%war_zone_send(M,Msg) ->
%%  case ets:lookup(?ETS_SHARE_DATA,center_node_list) of
%%    [] ->
%%      case util_master:is_master() of
%%        false ->
%%          throw(no_center);
%%        true ->
%%          ignore
%%      end;
%%    [#r_share_data{value = NodeList}] ->
%%      lists:foreach(fun(N) ->
%%        catch erlang:send({M,N},Msg)
%%                    end,NodeList)
%%  end.
%%
%%%%call中心节点
%%center_call(M,Msg) ->
%%  case ets:lookup(?ETS_SHARE_DATA,center_node) of
%%    [] ->
%%      case util_master:is_master() of
%%        false ->
%%          throw(no_center);
%%        true ->
%%          ignore
%%      end;
%%    [#r_share_data{value = CenterNode}] ->
%%      case CenterNode =:= node() andalso whereis(M) =:= self() of
%%        true ->   %%说明在call自己了
%%          throw(center_call_self);
%%        false ->
%%          gen_server:call({M,CenterNode},Msg)
%%      end
%%  end.
%%
%%pid(ServerID, RoleID) ->
%%  case catch normal_call(ServerID, war_zone_server, {is_online, RoleID}) of
%%    true -> true;
%%    _ -> false
%%  end.
%%
%%%%call普通节点
%%normal_call(ServerID,M,Msg) ->
%%  Node = node_server:get_node_info(ServerID),
%%  case whereis(M) =:= self() andalso Node =:= node() of
%%    true ->
%%      throw(normal_call_self);
%%    _ ->
%%      case Node of
%%        ?undefined ->
%%          throw(unknow_server_node);
%%        _ ->
%%          try
%%            gen_server:call({M,Node},Msg)
%%          catch
%%            exit:{noproc,_} ->
%%              {false,?err_system_server_down};
%%            Type:Reson ->
%%              ?ERR("normal_call error Type = ~w,Reson = ~w",[Type,Reson]),
%%              {false,?err_system}
%%          end
%%      end
%%  end.
%%
%%%%获取自己的服务器id
%%get_self_server_id() ->
%%  data_setting:get(server_id).
%%
%%%%获取服务器名称
%%get_server_name() ->
%%  data_setting:get(serverName).
%%
%%%%获取战区中的所有服务器的ID
%%get_all_war_server_id() ->
%%  case ets:lookup(?ETS_SHARE_DATA,war_zone_server_id_list) of
%%    [#r_share_data{value = ServerIDList}] ->
%%      ServerIDList;
%%    _ ->
%%      [get_self_server_id()]
%%  end.
%%
%%%%判断自己是否是中心节点
%%is_center() ->
%%  case util_master:is_master() of
%%    true ->
%%      false;
%%    false ->
%%      case ets:lookup(?ETS_SHARE_DATA,center_node) of
%%        [#r_share_data{value = CenterNode}] ->
%%          CenterNode == node();
%%        _ ->
%%          false
%%      end
%%  end.
%%
%%%%跨服给玩家发消息
%%send_role_msg(ServerID,RoleID,Msg) ->
%%  normal_send(ServerID,war_zone_server,{center_send_msg,RoleID,Msg}).
