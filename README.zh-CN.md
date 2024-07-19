# 简单的 erl 游戏服务器框架

## 编译
可使用 rebar3 来编译，或者 compile.bat 脚本生成

## 运行
start.bat 或者 start.sh

## client 模拟
进入client，执行run.bat，由 python 脚本简单实现

## protobuff、消息路由生成
在 proto 目录下，按 序号+模块名.proto（如1login.proto、2role.proto...）写好协议，点击 run.bat 生成
1. proto.py 生成路由文件 route.erl
2. protoc-erl 生成 erlang 协议文件
    > 编译 3rd\gpb 生成 protoc-erl 或直接使用 3rd\gpb\bin\protoc-erl
3. protoc 生成客户端协议文件

## 细节备注
gate 学习性的使用 gen_statem 行为，并使用 Ranch 作为 acceptor pool， 使用 gen_server 也要略做修改（参考框架使用的 gen_server 行为）

流程：一个 role 为一个线程，在 gate 上通过启动 role 来加载一个玩家，并使用 mysql 落地。

```erlang
role.erl gen_server
start(Args) ->%% Args 为外面传入的元素组成的列表
  supervisor:start_child(role_sup, Args).
  %% 对于 simple_one_for_one 的 supervisor，start => {'role', start_link, []}，apply(M,F,Args ++ []) 调用 role:start_link(列表展开)，Module:init(列表展开) 会被调用
```

## 感谢
[Ranch](https://github.com/ninenines/ranch)  
[mysql-otp-poolboy](https://github.com/mysql-otp/mysql-otp-poolboy)  
[lager](https://github.com/erlang-lager/lager) 最近上传的时候发现 lager 以前的 bug 已经修复，不用再本地 fork 维护了  
[gdb](https://github.com/tomas-abrahamsson/gpb)  
[dynamic_compile](https://github.com/jkvor/dynamic_compile)  
