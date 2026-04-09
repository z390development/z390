@if /I "%1" == "tron" (echo on) else (echo off)
REM Start zCICS transaction server in TRACE mode to create a Regression Test script

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
SET PROGRAM=Z390CICS
SET Z390INI=%CICSPATH%\%PROGRAM%.INI

REM get the z390 directory (2 levels up: z390\cics\bat)
set "zdir=%~dps0..\..\"
for %%f in ("%zdir%") do set "zdir=%%~ff"
REM remove trailing forward slash
set zdir=%zdir:~0,-1%
REM z390.jar in z390 directory
set JARPATH=%zdir%

set Z390SEQ=%CICSPATH%\seq\

COPY %CICSPATH%\Z390TRC.INI %CICSPATH%\Z390CICS.INI /Y >NUL
CALL %CICSPATH%\bat\Z390FILE
CALL bat\EZ390 %z_TraceMode% %CICSPATH%\%PROGRAM% NOTIME %1 %2 %3 %4 CON
IF ERRORLEVEL 1 GOTO WRONG
PAUSE Verify startup of zCICS

set z_ReturnCode=0
GOTO return

:WRONG
echo %0 error while starting zCICS
set z_ReturnCode=8

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
