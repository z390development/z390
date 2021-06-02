rem link to generate 390 load module from 1 or more obj files
rem 2019-07-22 RPI 1636 Abe Kornelis - Shift in .bat files not handled correctly - option /1 added
echo off
if %1 == tron ECHO ON
if %1 == tron shift /1
if %1 == TRON ECHO ON
if %1 == TRON shift /1
if exist %1.390 erase %1.390
if exist %1.LST erase %1.LST
if exist %1.ERR erase %1.ERR
if exist %1.STA erase %1.STA
if exist %1.TR* erase %1.TR*
call %~dps0lz390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto lzerr
goto end
:lzerr
pause see errors on lz390 generated lst file and console
:end
