@echo off

call base.bat %SERVER_NODE% %SERVER_COOKIE%
set path=C:\Program Files\Erlang OTP\bin;%path%
start werl -name debug_%SERVER_NODE% -setcookie %SERVER_COOKIE% -pa app 3rd/ebin ebin setting -config app/app -mnesia dir mnesia_data
