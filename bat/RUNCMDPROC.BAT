@if /I %1. == tron. (echo on) else (echo off)
rem run programs that test CMDPROC (SVC 34)

setlocal
if /I %1. == tron. (set z_TraceMode=tron
                    shift /1
            ) else (if /I %1. == troff. (set z_TraceMode=troff
                                         shift /1
                                 ) else (set z_TraceMode=)
                    )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

pushd %~dp0\..

rem Test one Command Processor
set z_testID=1
call bat\asmlg %z_TraceMode% rt\test\TESTCMD3 || goto error

rem Test two Command Processors (3 tests)
rem A. Both have same number of lines of output
rem B Second CP has more lines of output
rem C. First CP has more lines of output

set z_testID=2A
call bat\asmlg %z_TraceMode% rt\test\TESTCMD4 PARM(1) || goto error
set z_testID=2B
call bat\asmlg %z_TraceMode% rt\test\TESTCMD4 PARM(2) || goto error
set z_testID=2C
call bat\asmlg %z_TraceMode% rt\test\TESTCMD4 PARM(3) || goto error
set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% in test %z_testID% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
