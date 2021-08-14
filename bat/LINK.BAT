@echo off
setlocal
rem link to generate 390 load module from 1 or more obj files
if %1 == tron ECHO ON
if %1 == tron shift
if %1 == TRON ECHO ON
if %1 == TRON shift
if exist %1.390 erase %1.390
if exist %1.LST erase %1.LST
if exist %1.ERR erase %1.ERR
if exist %1.STA erase %1.STA
if exist %1.TR* erase %1.TR*
call %~dps0LZ390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if %ERRORLEVEL% NEQ 0 (
    echo LINK     ERROR: See errors on lz390 generated lst file and console
    exit /b %ERRORLEVEL%
)