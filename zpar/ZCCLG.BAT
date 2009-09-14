rem ZCCLG translate CBL to MLC, assemble, link, and exec with no pauses 
echo off
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
call \work\z390\ZC390 %1 SYSCPY(w:\work\z390\zcobol+w:\work\z390\zcobol\z390+w:\work\nist\src) %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto end
call \work\z390\MZ390 %1  @\work\z390\zcobol\z390\ZC390CLG %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto end
call \work\z390\LZ390 %1  @\work\z390\zcobol\z390\ZC390CLG %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto end
set SYSIN=%1.IN
set SYSOUT=%1.OUT
call \work\z390\EZ390 %1  @\work\z390\zcobol\z390\ZC390CLG %2 %3 %4 %5 %6 %7 %8 %9
:end 
