@echo off
setlocal
if /I %1. == tron. (echo on
                    shift /1
                    )
if /I %1. == .     (echo %0 ERROR: missing file name
                    exit /b 16
                    )

rem exec execute 390 load module
if exist %1.LOG erase %1.LOG
if exist %1.ERR erase %1.ERR
if exist %1.STA erase %1.STA
if exist %1.TR* erase %1.TR*
call %~dps0ez390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if %ERRORLEVEL% NEQ 0 (
    echo EXEC     ERROR: RC %ERRORLEVEL% See errors on ez390 generated log file and console
    exit /b %ERRORLEVEL%
)
