@if /I "%1" == "tron" (echo on) else (echo off)
REM Start launcher of z390/cics remote terminals

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

SET CICSPATH=cics
SET PROGRAM=Z390RMTE
CALL bat\EZ390 %z_TraceMode% %CICSPATH%\%PROGRAM% SYS390(+zcobol\lib) ASCII NOTIME PARM(%1) %2 %3
IF ERRORLEVEL 1 GOTO WRONG

set z_ReturnCode=0
GOTO return

:WRONG
echo %0 error while starting zCICS
set z_ReturnCode=8

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
