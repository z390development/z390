rem asm run macro assembly to generate relocatible obj from mlc source 
echo off
if %1 == tron ECHO ON
if %1 == tron shift
if %1 == TRON ECHO ON
if %1 == TRON shift
if exist %1.BAL erase %1.BAL
if exist %1.PRN erase %1.PRN
if exist %1.OBJ erase %1.OBJ
if exist %1.ERR erase %1.ERR
if exist %1.STA erase %1.STA
if exist %1.TR* erase %1.TR*

rem get the z390 directory
set "zdir=%~dps0..\"
for %%f in ("%zdir%") do set "zdir=%%~ff"

call %~dps0mz390 %1  sysmac(+%zdir%mac) syscpy(+%zdir%mac) %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto mzerr
goto end
:mzerr
erase %1.OBJ
pause see errors on mz390 generated bal file and console
:end
