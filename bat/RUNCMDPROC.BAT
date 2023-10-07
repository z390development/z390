@if /I "%1" == "tron" (echo on) else (echo off)
rem run programs that test CMDPROC (SVC 34)

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

pushd %~dps0..

set RTPATH=rt\test

rem Test one Command Processor
set z_testID=1

rem name of test file
set TESTCMDFILE=%RTPATH%\bat\TESTCMD3.BAT

call bat\asmlg %z_TraceMode% %RTPATH%\TESTCMD3 || goto error

rem Test two Command Processors (3 tests)
rem A. Both have same number of lines of output
rem B Second CP has more lines of output
rem C. First CP has more lines of output

set z_testID=2A

rem names of test files for first test
set TESTCMDFILE1=%RTPATH%\bat\TESTCMD4A.BAT
set TESTCMDFILE2=%RTPATH%\bat\TESTCMD4B.BAT

call bat\asmlg %z_TraceMode% %RTPATH%\TESTCMD4 || goto error
set z_testID=2B

rem names of test files for second test
set TESTCMDFILE1=%RTPATH%\bat\TESTCMD4A.BAT
set TESTCMDFILE2=%RTPATH%\bat\TESTCMD4C.BAT

call bat\asmlg %z_TraceMode% %RTPATH%\TESTCMD4 || goto error
set z_testID=2C

rem names of test files for third test
set TESTCMDFILE1=%RTPATH%\bat\TESTCMD4C.BAT
set TESTCMDFILE2=%RTPATH%\bat\TESTCMD4B.BAT

call bat\asmlg %z_TraceMode% %RTPATH%\TESTCMD4 || goto error
set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% in test %z_testID% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
