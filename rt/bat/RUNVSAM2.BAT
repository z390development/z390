@if /I "%1" == "tron" (echo on) else (echo off)
rem rebuild and test sort utility

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

call bat\asml %z_TraceMode% vsam2\mlc\Z390CAT2 trace sysmac(mac) noloadhigh zvsam(2) || goto error
call bat\asml %z_TraceMode% vsam2\mlc\ZVSAM19C trace sysmac(mac) noloadhigh zvsam(2) || goto error
call bat\asml %z_TraceMode% vsam2\mlc\TESTGNCB trace sysmac(mac) noloadhigh zvsam(2) || goto error
call bat\asml %z_TraceMode% vsam2\mlc\TESTMDCB trace sysmac(mac) noloadhigh zvsam(2) || goto error
call bat\asml %z_TraceMode% vsam2\mlc\TESTSHCB trace sysmac(mac) noloadhigh zvsam(2) || goto error
call bat\asml %z_TraceMode% vsam2\mlc\TESTTSCB trace sysmac(mac) noloadhigh zvsam(2) || goto error

rem if we get here, there were no errors
echo Verify all sort tests ok
goto return

:error
echo %0 ERROR: Test program failed to assemble. See log and listings for details.
set z_ReturnCode=8

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
