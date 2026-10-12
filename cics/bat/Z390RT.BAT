@if /I "%1" == "tron" (echo on) else (echo off)
REM Start zCICS transaction server in Regression Test mode

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

COPY %CICSPATH%\Z390RT.INI %CICSPATH%\Z390CICS.INI /Y >NUL
CALL %CICSPATH%\bat\Z390FILE
rem -- parms below are defaults for interactive use. Override using positional parameters on call
CALL bat\EZ390 %z_TraceMode% %CICSPATH%\%PROGRAM% NOTIME MAXQUE(2048000) %1 %2 %3 %4 CON
set z_ReturnCode=%ERRORLEVEL%
rem -- returncode 16 is expected - not showing that to avoid confusion
echo #########################################################
echo #
echo # %0 zCICS regression test suite completed
echo #
echo #########################################################

rem -- run the comparator
call cics\bat\Z390CMPG.BAT ASCII
set z_ReturnCode=%ERRORLEVEL%
echo #########################################################
echo #
echo # %0 zCICS regression test comparator comlete
echo # %0 error level is %z_ReturnCode%
echo #
echo #########################################################
if %z_ReturnCode% GTR 0 goto return

rem -- ensure the differences file is empty
for %%A in (%SEQOUT%) do if %%~zA GTR 0 (
    echo %0 differences written to %SEQOUT%
    set z_ReturnCode=8
    goto return
)
set z_ReturnCode=0

rem -- undo changed made to run the test
COPY %CICSPATH%\Z390RUN.INI %CICSPATH%\Z390CICS.INI /Y >NUL
DEL  %Z390SEQ%\SEQO0001.TXT /Q >NUL

:return
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
