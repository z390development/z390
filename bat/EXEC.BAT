echo off
rem 2019-07-22 RPI 1636 Abe Kornelis - Shift in .bat files not handled correctly - option /1 added
if %1 == tron ECHO ON
if %1 == tron shift /1
if %1 == TRON ECHO ON
if %1 == TRON shift /1
rem exec execute 390 load module
if exist %1.LOG erase %1.LOG
if exist %1.ERR erase %1.ERR
if exist %1.STA erase %1.STA
if exist %1.TR* erase %1.TR*
call %~dps0ez390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto ezerr
goto end
:ezerr
pause see errors on ez390 generated log file and console
:end
