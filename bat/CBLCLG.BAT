rem CBLCLG translate CBL to MLC, assemble, link, and exec using z390 
echo off
if %1 == tron ECHO ON
if %1 == tron shift
if %1 == TRON ECHO ON
if %1 == TRON shift
if exist %1.CBL goto erase
echo on
pause no zcobol program found %1.CBL
goto end
:erase
if exist %1.MLC erase %1.MLC
if exist %1.390 erase %1.390
if exist %1.BAL erase %1.BAL
if exist %1.ERR erase %1.ERR
if exist %1.LST erase %1.LST
if exist %1.OBJ erase %1.OBJ
if exist %1.PRN erase %1.PRN
if exist %1.STA erase %1.STA
if exist %1.390 erase %1.390
if exist %1.cpp erase %1.cpp
if exist %1.java  erase %1.java    
if exist %1.class erase %1.class   
call %~dps0ZC390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto mzerr
call %~dps0MZ390 %1 @%~dps0cblopt sysmac(zcobol\mac+mac) syscpy(+zcobol\cpy) %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto mzerr
call %~dps0LZ390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto lzerr
call %~dps0EZ390 %1 sys390(+zcobol\lib) %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto ezerr
goto end
:mzerr
pause see errors on mz390 generated bal file and console
goto end
:lzerr
pause see errors on lz390 generated lst file and console
goto end
:ezerr
pause see errors on ez390 generated log file and console
:end 
