# Simple erl game server framework[中文](https://github.com/anonymou3/eserver/blob/master/README.zh-CN.md)

## Compile
Compiled by rebar3, or execute the compile.bat script.

## Run
start.bat or start.sh

## client mock
Go to client and execute run.bat, which is simply implemented by python script.

## protobuff, message route generation
In the proto directory, press serial number + module name.proto (e.g. 1login.proto, 2role.proto...) Write the protocol and click run.bat to generate
1. proto.py to generate the route file route.erl
2. protoc-erl to generate erlang protocol file
    > Compile 3rd\gpb to generate protoc-erl or use 3rd\gpb\bin\protoc-erl.
3. protoc generate client protocol file

## Details
gate learns to use gen_statem behavior and uses Ranch as acceptor pool, and uses gen_server with minor modifications (see gen_server behavior used by the framework)

Process: one role for one thread, load a player on the gate by starting the role, and use mysql to store them.

```erlang
role.erl gen_server
start(Args) ->%% Args is a list of elements passed in from outside
  supervisor:start_child(role_sup, Args).
  %% For simple_one_for_one supervisor, start => {'role', start_link, []}, apply(M,F,Args ++ []) call role:start_link(list_expanded), Module:init(list_expanded) will be called
```

### Thanks to
[Ranch](https://github.com/ninenines/ranch)  
[mysql-otp-poolboy](https://github.com/mysql-otp/mysql-otp-poolboy)  
[lager](https://github.com/erlang-lager/lager) Recently when I uploaded, I realized that lager's previous bugs have been fixed, so I don't need to fork it locally anymore.  
[gdb](https://github.com/tomas-abrahamsson/gpb)  
[dynamic_compile](https://github.com/jkvor/dynamic_compile)  
