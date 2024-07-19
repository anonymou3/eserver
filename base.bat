@echo off

set SERVER_NODE=%~1
set SERVER_COOKIE=%~2
set SH_PATH="%~dp0"
set server_id=
set server_type=
set server_host=

for /f "eol=%% tokens=1,2,3* delims={,} " %%i in (setting\data_setting.config) do (
    if %%i==server_id (
        set "%%i=%%j"
    ) else if %%i==server_type (
        set "%%i=%%j"
    ) else if %%i==server_host (
        set "%%i=%%j"
        goto :break
    )
)

:break
rem del "" from the string
set server_host=%server_host:"=%

set SERVER_HOST=%server_host%
set SERVER_ID=%server_id%
set NODE_TYPE=%server_type%
set NODE_COOKIE=test

set SERVER_NODE=%NODE_COOKIE%_game_%NODE_TYPE%_%SERVER_ID%@%SERVER_HOST%
set SERVER_COOKIE=%NODE_COOKIE%_game
