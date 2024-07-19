@echo off

rem clean log
del /f /s /q log\*.*
del /f /s /q ebin\*.*
del /f /s /q mnesia_data\*.*
del /f /s /q erl_crash.dump