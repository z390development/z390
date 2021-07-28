@echo off
rem asmlg assemble, link, and go from mlc to 390 execution
setlocal
if %1 == tron ECHO ON
if %1 == tron shift
if %1 == TRON ECHO ON
if %1 == TRON shift
if exist %1.BAL erase %1.BAL
if exist %1.PRN erase %1.PRN
if exist %1.OBJ erase %1.OBJ
if exist %1.LST erase %1.LST
if exist %1.390 erase %1.390
if exist %1.LOG erase %1.LOG
if exist %1.ERR erase %1.ERR
if exist %1.STA erase %1.STA
if exist %1.TR* erase %1.TR*

rem get the z390 directory
set "zdir=%~dps0..\"
for %%f in ("%zdir%") do set "zdir=%%~ff"

call %~dps0mz390 %1  sysmac(+%zdir%mac) syscpy(+%zdir%mac) %2 %3 %4 %5 %6 %7 %8 %9
if %ERRORLEVEL% NEQ 0 (
    echo ASMLG    ERROR: See errors on mz390 generated bal file and console
    exit /b %ERRORLEVEL%
)
call %~dps0LZ390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if %ERRORLEVEL% NEQ 0 (
    echo ASMLG    ERROR: see errors on lz390 generated lst file and console
    exit /b %ERRORLEVEL%
)
call %~dps0EZ390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if %ERRORLEVEL% NEQ 0 (
    echo ASMLG    ERROR: see errors on ez390 generated log file and console
    exit /b %ERRORLEVEL%
)
