-ifndef(def_migrate_hrl).
-define(def_migrate_hrl, true).

-define(migrate, migrate).
-define(migrate_record, migrate_record).
-define(log_migrate, log_migrate).
-define(log_migrate_record, log_migrate_record).

-define(migrate_do_list, [
  %% 可以考虑把表全导出建表放在一个函数中，从而启动时生成表
  {?migrate, clear_except_migration}
  , {?migrate, create_tables}
  , {?migrate, create_g_role_misc_2023_3_16}
  %%  , {?migrate_record, record_operations_qian_2020_12_1}
]).


%%please  keep  endif at the tail of the file
-endif.
