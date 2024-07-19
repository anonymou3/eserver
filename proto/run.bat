@echo off

cd /d %~dp0

rem gen route.erl
python proto.py

rem gen erl proto
set path=C:\Program Files\Erlang OTP\bin;%path%
set compiler=%~dp0\..\3rd\gpb\bin\protoc-erl
escript %compiler% -I. all.proto

copy /b /y *.erl %~dp0\..\src\proto\
copy /b /y all.hrl %~dp0\..\include\

rem gen python proto
protoc --python_out=./ all.proto
copy /b /y all_pb2.py %~dp0\..\client
pause
