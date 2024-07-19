@echo off

if not exist ebin (mkdir ebin)
set path="C:\Program Files\Erlang OTP\bin";%path%

cd /d "%~dp0"
start werl +K true +t 5000000 +Q 1048576 +P 1048576 -pa 3rd/ebin ebin app -config app/app -s make all
