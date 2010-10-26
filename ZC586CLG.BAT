rem ZC586CLG translate CBL to HLA, assemble/link to EXE via HLA, and exec 
echo off
if %1 == tron ECHO ON
if %1 == tron shift
if %1 == TRON ECHO ON
if %1 == TRON shift
if exist %1.MLC erase %1.MLC
if exist %1.390 erase %1.390
if exist %1.BAL erase %1.BAL
if exist %1.ERR erase %1.ERR
if exist %1.LST erase %1.LST
if exist %1.OBJ erase %1.OBJ
if exist %1.PRN erase %1.PRN
if exist %1.STA erase %1.STA
if exist %1.HLA erase %1.HLA
if exist %1.ASM erase %1.ASM
if exist %1.EXE erase %1.EXE
if exist %1.cpp erase %1.cpp
if exist %1.java  erase %1.java    
if exist %1.class erase %1.class   
call %~dps0ZC390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto mzerr
set HLAOUT=%1.HLA
call %~dps0MZ390 %1 @zcobol\i586\ZC586CLG %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto mzerr
call zcobol\i586\CLG %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto clgerr
goto end
:mzerr
pause see errors on mz390 generated bal file and console
goto end
:clgerr
pause see HLA/MSASM CLG errors 
goto end
:end 
