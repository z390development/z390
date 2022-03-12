@if /I "%1" == "tron" (echo on) else (echo off)
rem regression test z390 instructions

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
set SNAPOUT=ZOPCHECK\SNAPOUT.TXT
call bat\asmlg %z_TraceMode% zopcheck\ZOPCHECK bal trace sysmac(mac) syscpy(zopcheck+mac) optable(z390) %1 %2 %3
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (
                         echo %0 ERROR: ASMLG returned non-zero result
                         goto return
                         )
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
