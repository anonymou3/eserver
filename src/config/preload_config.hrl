-ifndef(preload_config_hrl).
-define(preload_config_hrl, true).

-define(PRELOAD_CONFIG, [
  %%  绝对路径/源文件名  erlang中的实际名 生成方式
  %% 服务器通用配置
  {"setting/data_setting.config", data_setting, key_value, original}
  , {"config/data_common.config", data_common, key_value, original}
  %%  ,{"config/data_player_exp.config", data_player_exp, if_clause, original}
  %%  ,{"config/data_shadow.config", data_shadow, key_value, fun shadow:config/1}

]).
%%please  keep  endif at the tail of the file 
-endif.