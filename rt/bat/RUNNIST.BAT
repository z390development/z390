
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
if %1. == . goto no_repo

call bat\CBLCLG %z_TraceMode% %1\src\CM101M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\CM102M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\CM103M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\CM104M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\CM105M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\CM201M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\CM202M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\CM303M %2 %3 %4 %5 %6 %7 %8 %9
rem -- CM401M tests error conditions
call bat\CBLC   %z_TraceMode% %1\src\CM401M %2 %3 %4 %5 %6 %7 %8 %9
set z_ReturnCode=0
goto return

:no_repo
set z_ReturnCode=8
echo %0 ERROR: Argument 1 should have been path to NIST repository - cannot run tests
:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
