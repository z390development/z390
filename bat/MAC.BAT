@echo off
setlocal
pushd %~dp0
cd ..
rem mac run mz390 macro processor to expand mlc to bal with nomlc
if %1 == tron ECHO ON
if %1 == tron shift
if %1 == TRON ECHO ON
if %1 == TRON shift
if exist %1.BAL erase %1.BAL
if exist %1.ERR erase %1.ERR
if exist %1.STA erase %1.STA
if exist %1.TR* erase %1.TR*
call %~dps0mz390 %1  bal noasm sysmac(%~dps0mac+.) syscpy(%~dps0mac+.) chksrc(0) chkmac(0) %2 %3 %4 %5 %6 %7 %8 %9
if %ERRORLEVEL% NEQ 0 (
    echo MAC      ERROR: Encountered RC %ERRORLEVEL% - exiting
)
