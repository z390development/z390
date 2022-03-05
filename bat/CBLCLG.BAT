@echo off
rem CBLCLG translate CBL to MLC, assemble, link, and exec using z390 
setlocal
if /I %1. == tron. (echo on
                    shift /1
                    )
if /I %1. == .     (echo %0 ERROR: missing file name
                    exit /b 16
                    )

if not exist %1.CBL (
    echo CBLCLG   ERROR: zCOBOL program not found %1.CBL
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
    echo CBLCLG   ERROR: COBOL compile error
    exit /b %ERRORLEVEL%
)
rem get the z390 directory
set "zdir=%~dps0..\"
for %%f in ("%zdir%") do set "zdir=%%~ff"

call %~dps0MZ390 %1  @%zdir%zcobol\opt\CBLOPT sysmac(%zdir%zcobol\mac+mac) syscpy(+%zdir%zcobol\cpy) %2 %3 %4 %5 %6 %7 %8 %9
if %ERRORLEVEL% NEQ 0 (
    echo CBLCLG   ERROR: See errors on mz390 generated bal file and console
    exit /b %ERRORLEVEL%
)

call %~dps0LZ390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if %ERRORLEVEL% NEQ 0 (
    echo CBLCLG   ERROR: See errors on lz390 generated lst file and console
    exit /b %ERRORLEVEL%
)

call %~dps0EZ390 %1 sys390(+%zdir%zcobol\lib) %2 %3 %4 %5 %6 %7 %8 %9
if %ERRORLEVEL% NEQ 0 (
    echo CBLCLG   ERROR: See errors on ez390 generated log file and console
    exit /b %ERRORLEVEL%
)
