@if /I "%1" == "tron" (echo on) else (echo off)
rem CBLC translate CBL to MLC using zc390 and assemble using mz390 

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

if /I %1. == . (echo %0 ERROR: program name not specified
                set z_ReturnCode=16
                goto return
                )

if not exist %1.CBL (echo %0 ERROR: specified program %1.CBL not found
                     set z_ReturnCode=16
                     goto return
                     )

if exist %1.MLC   erase %1.MLC
if exist %1.390   erase %1.390
if exist %1.BAL   erase %1.BAL
if exist %1.ERR   erase %1.ERR
if exist %1.LST   erase %1.LST
if exist %1.OBJ   erase %1.OBJ
if exist %1.PRN   erase %1.PRN
if exist %1.STA   erase %1.STA
if exist %1.390   erase %1.390
if exist %1.cpp   erase %1.cpp
if exist %1.java  erase %1.java
if exist %1.class erase %1.class
if exist %1_ZC_LABELS.CPY erase %1_ZC_LABELS.CPY

rem get the z390 directory
set "z_HomeDir=%~dp0\..\"
for %%f in ("%z_HomeDir%") do set "z_HomeDir=%%~ff"

call %~dp0\ZC390 %z_TraceMode% %1 %2 %3 %4 %5 %6 %7 %8 %9
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% GTR 4 (echo %0 ERROR: Cobol compile error
                         goto return
                         )

call %~dp0\MZ390 %z_TraceMode% %1 @%z_HomeDir%zcobol\opt\CBLOPT sysmac(%z_HomeDir%zcobol\mac+mac) syscpy(+%z_HomeDir%zcobol\cpy) %2 %3 %4 %5 %6 %7 %8 %9
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% GTR 4 (echo %0 ERROR: see errors on mz390 generated bal file and console
                         goto return
                         )
:return
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
