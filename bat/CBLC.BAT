@echo off
rem CBLC translate CBL to MLC using zc390 and assemble using mz390 
setlocal
if %1 == tron ECHO ON
if %1 == tron shift
if %1 == TRON ECHO ON
if %1 == TRON shift
if not exist %1.CBL (
    echo CBLC     ERROR: zCOBOL program not found %1.CBL
    exit /b 1
)
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
if %ERRORLEVEL% NEQ 0 (
    echo CBLC     ERROR: COBOL compile error
    exit /b %ERRORLEVEL%
)

rem get the z390 directory
set "zdir=%~dps0..\"
for %%f in ("%zdir%") do set "zdir=%%~ff"

call %~dps0MZ390 %1  @%zdir%zcobol\opt\CBLOPT sysmac(%zdir%zcobol\mac+mac) syscpy(+%zdir%zcobol\cpy) %2 %3 %4 %5 %6 %7 %8 %9
rem call %~dps0MZ390 %1  @%~dps0CBLOPT sysmac(zcobol\mac+mac) syscpy(+zcobol\cpy) %2 %3 %4 %5 %6 %7 %8 %9
if %ERRORLEVEL% NEQ 0 (
    echo CBLC     ERROR: See errors on mz390 generated bal file and console
    exit /b %ERRORLEVEL%
)
