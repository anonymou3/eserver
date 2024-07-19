@echo off

call base.bat %SERVER_NODE% %SERVER_COOKIE%
set path=C:\Program Files\Erlang OTP\bin;%path%

erl_call -c %SERVER_COOKIE% -n %SERVER_NODE% -a "user_default stop []"
