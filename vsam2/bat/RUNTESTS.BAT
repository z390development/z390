@if /I "%1" == "tron" (echo on) else (echo off)
rem assemble, link test programs and files for zvsam(2)
rem Argument 1 is expected to point to a local clone of the z390Archive repository

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

pushd %~dps0..\..

REM GENCB test
set ERRMSG=TESTGNCB assembly/link error
call vsam2\bat\TESTGNCB %z_TraceMode% zVSAM(2) || goto errmsg

REM MODCB test
set ERRMSG=TESTMDCB assembly/link error ||  goto errmsg
call vsam2\bat\TESTMDCB %z_TraceMode% zVSAM(2) || goto errmsg

REM SHOWCB test
set ERRMSG=TESTSHCB assembly/link error
call vsam2\bat\TESTSHCB %z_TraceMode% zVSAM(2) || goto errmsg

REM TESTCB test
set ERRMSG=TESTTSCB assembly/link error
call vsam2\bat\TESTTSCB %z_TraceMode% zVSAM(2) || goto errmsg

REM zREPRO tests for LOAD/UUNLOAD/VERIFY/BUILDAIX
call vsam2\bat\TSTLOAD.BAT %z_TraceMode% "%~1" || goto error

set RC=0
goto return

:errmsg
echo ERROR: %ERRMSG%
set RC=8

:return
if exist vsam2\%SORTWK*.TMP DEL vsam2\SORTWK*.TMP
popd
exit /b %RC%
