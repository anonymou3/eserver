@echo off

if exist ebin (del /f /s /q ebin\*)

echo mysql-otp-poolboy used rebar3, can not compile it
copy /b /y mysql-otp-poolboy\_build\default\lib\mysql\ebin\*.beam ebin\
copy /b /y mysql-otp-poolboy\_build\default\lib\mysql_poolboy\ebin\*.beam ebin\
copy /b /y mysql-otp-poolboy\_build\default\lib\poolboy\ebin\*.beam ebin\

if not exist ebin (mkdir ebin)
set path="C:\Program Files\Erlang OTP\bin"
erl -make
pause