@echo off

set where=%~dp0
%1 %2
mshta vbscript:createobject("shell.application").shellexecute("%~s0","goto :admin_do","","runas",1)(window.close)&exit
:admin_do

cd /d %where%
if not exist ./log (mkdir log)

call base.bat %SERVER_NODE% %SERVER_COOKIE%
echo %SERVER_NODE% %SERVER_COOKIE%
set path=C:\Program Files\Erlang OTP\bin;%path%
start werl.exe -name %SERVER_NODE% +t 100000000 +Q 1048576 +P 1048576 -setcookie %SERVER_COOKIE% -pa app 3rd/ebin ebin setting -config app/app -mnesia dir mnesia_data -s main start
