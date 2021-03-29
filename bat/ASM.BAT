rem asm run macro assembly to generate relocatible obj from mlc source 
echo off
if %1 == tron ECHO ON
if %1 == tron shift
if %1 == TRON ECHO ON
if %1 == TRON shift
if exist %1.bal erase %1.bal
if exist %1.prn erase %1.prn
if exist %1.obj erase %1.obj
if exist %1.err erase %1.err
if exist %1.sta erase %1.sta
if exist %1.tr* erase %1.tr*
call %~dps0mz390 %1  sysmac(%~dps0mac+.) syscpy(%~dps0mac+.) %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto mzerr
goto end
:mzerr
erase %1.obj
pause see errors on mz390 generated bal file and console
:end
