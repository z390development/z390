rem link to generate 390 load module from 1 or more obj files
echo off
if %1 == tron ECHO ON
if %1 == tron shift
if %1 == TRON ECHO ON
if %1 == TRON shift
if exist %1.390 erase %1.390
if exist %1.lst erase %1.lst
if exist %1.err erase %1.err
if exist %1.sta erase %1.sta
if exist %1.tr* erase %1.tr*
call %~dps0lz390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto lzerr
goto end
:lzerr
pause see errors on lz390 generated lst file and console
:end
