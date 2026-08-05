@if /I "%1" == "tron" (echo on) else (echo off)
rem run MainFrame Assembler Coding Contest solutions

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
rem -- For now: assembling and running; not validating output SNAP content
call bat\ASMLG %z_TraceMode% rt\test\TESTACT1 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% rt\test\TESTAIN1 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% rt\test\TESTAIN2 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% rt\test\TESTAIN3 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% rt\test\TESTASM1 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% rt\test\TESTASM2 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% rt\test\TESTCAL1 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% rt\test\TESTCAL2 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% rt\test\TESTCDE1 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% rt\test\TESTCFD1 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error

call bat\ASML  %z_TraceMode% rt\test\TESTSUB1 notiming stats bal            %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error

rem -- TESTBLD1 requires rt\test\TESTSUB1.390 and mvs\demo\DEMO.390
if not exist mvs\demo\DEMO.390 call bat\ASML %z_TraceMode% mvs\demo\DEMO notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% rt\test\TESTBLD1 notiming stats bal noloadhigh sys390(rt\test+mvs\demo) %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error

set z_ReturnCode=0
goto return

:error
if %ERRORLEVEL% EQU 0 (set z_ReturnCode=6
               ) else (set z_ReturnCode=%ERRORLEVEL%)
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
