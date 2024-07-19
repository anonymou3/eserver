@echo off

echo mysql-otp-poolboy used rebar3, can not compile it

if not exist ebin (mkdir ebin)
set path="C:\Program Files\Erlang OTP\bin"
erl -make
pause