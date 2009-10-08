rem asmlg assemble, link, and go from mlc to 390 execution
echo off
if exist /usr/local/lib/z390/z390.jar goto linux
if %1 == tron ECHO ON
if %1 == tron shift
if %1 == TRON ECHO ON
if %1 == TRON shift
:linux
if exist %1.BAL erase %1.BAL
if exist %1.PRN erase %1.PRN
if exist %1.OBJ erase %1.OBJ
if exist %1.LST erase %1.LST
if exist %1.390 erase %1.390
if exist %1.LOG erase %1.LOG
if exist %1.ERR erase %1.ERR
if exist %1.STA erase %1.STA
if exist %1.TR* erase %1.TR*
call %~dps0MZ390 %1  sysmac(%~dps0mac+.) syscpy(%~dps0mac+.) %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto mzerr
call %~dps0LZ390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto lzerr
call %~dps0EZ390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto ezerr
goto end
:mzerr
pause see errors on mz390 generated bal file and console
goto end
:lzerr
pause see errors on lz390 generated lst file and console
erase %1.390
goto end
:ezerr
pause see errors on ez390 generated log file and console
:end 
