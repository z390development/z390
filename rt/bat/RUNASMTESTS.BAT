@if /I %1. == tron. (echo on) else (echo off)
rem regression test z390 instructions

setlocal
if /I %1. == tron. (set z_TraceMode=tron
                    shift /1
            ) else (if /I %1. == troff. (set z_TraceMode=troff
                                         shift /1
                                 ) else (set z_TraceMode=)
                    )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

pushd %~dp0
cd ..\..

call bat\asm   %z_TraceMode% tests\testins1 trace sysmac(mac) noloadhigh               || goto error
call bat\asmlg %z_TraceMode% tests\testins2 trace sysmac(mac) noloadhigh               || goto error
call bat\asmlg %z_TraceMode% tests\testins3 trace sysmac(mac) noloadhigh               || goto error
call bat\asmlg %z_TraceMode% tests\testins4 trace sysmac(mac) noloadhigh               || goto error
call bat\asmlg %z_TraceMode% tests\testins5 trace sysmac(mac) noloadhigh optable(z390) || goto error
call bat\asmlg %z_TraceMode% tests\testdfp1 trace sysmac(mac) noloadhigh optable(z390) || goto error
call bat\asmlg %z_TraceMode% tests\testdfp2 trace sysmac(mac) noloadhigh optable(z390) || goto error
call rt\bat\ZOPCHECK %z_TraceMode%                                                     || goto error
set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
