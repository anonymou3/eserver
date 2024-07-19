%%%-------------------------------------------------------------------
%%% @author new
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 6月 2022 14:11
%%%-------------------------------------------------------------------
-author("new").
-ifndef(errcode).
-define(errcode, true).

-define(err_ok, 0).
-define(err_login_account_exist, 1).
-define(err_kick_login_again, 1).                 %%重复登录
-define(err_kick_ban, 2).                 %%禁止登陆
-define(err_kick_heart_fast, 3).                 %%心跳过快
-define(err_kick_other, 4).                 %%未知的踢人方式

%%login
-define(err_login_ip_banned,  1).

-endif.
