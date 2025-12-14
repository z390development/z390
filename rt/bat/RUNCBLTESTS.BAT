@if /I "%1" == "tron" (echo on) else (echo off)
rem run z390 zcobol sample regression tests

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
call bat\CBLCLG %z_TraceMode% zcobol\tests\testadd1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\testadd2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
rem TESTCAL3 statically calls TESTASM4 - link them together
call bat\ASM    %z_TraceMode% zcobol\tests\TESTASM4 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCL  %z_TraceMode% zcobol\tests\TESTCAL3 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
rem TESTCAL1 statically calls TESTCAL2 and dynamically calls TESTCAL3
call bat\CBLC   %z_TraceMode% zcobol\tests\TESTCAL2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTCAL1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error

call bat\CBLCLG %z_TraceMode% zcobol\tests\testbfp1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\testif1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\testif2  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\testif3  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\testmov1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLClG %z_TraceMode% zcobol\tests\testmov2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\testmov3 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLC   %z_TraceMode% zcobol\tests\CM101M03 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\test\youzpi1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\CM101M01 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
