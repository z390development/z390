rem mac run mz390 macro processor to expand mlc to bal with nomlc
rem echo off
if %1 == tron ECHO ON
if %1 == tron shift
if %1 == TRON ECHO ON
if %1 == TRON shift
if exist %1.bal erase %1.bal
if exist %1.err erase %1.err
if exist %1.sta erase %1.sta
if exist %1.tr* erase %1.tr*
call %~dps0mz390 %1  bal noasm sysmac(%~dps0mac+.) syscpy(%~dps0mac+.) chksrc(0) chkmac(0) %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto err
goto end
:err
pause see mz390 bal errors
:end
