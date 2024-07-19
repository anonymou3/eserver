@echo off

if exist ebin (del /f /s /q ebin\*)
if not exist ebin (mkdir ebin)
set path="C:\Program Files\Erlang OTP\bin"

erl -pa 3rd/ebin -make
pause