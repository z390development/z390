@if /I "%1" == "tron" (echo on) else (echo off)
rem assemble, link test programs and files for zvsam(2)

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

rem assemble and link utilities
rem -- zVSAM V2 introduces the OCPL to allow Open/Close of muiltiple files
rem -- but that code has not been written yet for sz390.java's open/close handling.
set ERRMSG=assembly/link error
call bat\asml vsam2\mlc\ZREPRO   zVSAM(1) || goto errmsg
call bat\asml vsam2\mlc\ZVSAM19C zVSAM(2) || goto errmsg
call bat\asml vsam2\mlc\Z390CAT2 zVSAM(2) || goto errmsg

REM GENCB test
set ERRMSG=TESTGNCB assembly/link error
call vsam2\bat\TESTGNCB zVSAM(2) || goto errmsg

REM MODCB test
set ERRMSG=TESTMDCB assembly/link error ||  goto errmsg

REM SHOWCB test
set ERRMSG=TESTSHCB assembly/link error
call vsam2\bat\TESTSHCB zVSAM(2) || goto errmsg

REM TESTCB test
set ERRMSG=TESTTSCB assembly/link error
call vsam2\bat\TESTTSCB zVSAM(2) || goto errmsg

set RC=0
goto return

:errmsg
echo ERROR: %ERRMSG%
set RC=8

:return
if exist vsam2\%SORTWK*.TMP DEL vsam2\SORTWK*.TMP
popd
exit /b %RC%
