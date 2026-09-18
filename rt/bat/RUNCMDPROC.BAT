@if /I "%1" == "tron" (echo on) else (echo off)
rem run MVS demo and test programs

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
call bat\ASMLG %z_TraceMode% rt\test\TESTCMD1 notiming stats bal noloadhigh sysmac(mac) %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
findstr /c:"HELLO FOR LAST TIME" rt\test\TESTCMD1.LOG >nul || goto error

rem TESTCMD2 dropped - Windows-only version preceding TESTCMD4. The latter tests more variations.

set TESTCMDFILE=rt\test\bat\TESTCMD3.BAT
call bat\ASMLG %z_TraceMode% rt\test\TESTCMD3 notiming stats bal noloadhigh sysmac(mac) %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error

rem TESTCMD4: two command processors (3 pairings)
rem 1. Both CPs produce the same number of lines
set TESTCMDFILE1=rt\test\bat\TESTCMD4A.BAT
set TESTCMDFILE2=rt\test\bat\TESTCMD4B.BAT
call bat\ASMLG %z_TraceMode% rt\test\TESTCMD4 notiming stats bal noloadhigh sysmac(mac) %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
rem 2. Second CP produces more lines than first
set TESTCMDFILE1=rt\test\bat\TESTCMD4A.BAT
set TESTCMDFILE2=rt\test\bat\TESTCMD4C.BAT
call bat\ASMLG %z_TraceMode% rt\test\TESTCMD4 notiming stats bal noloadhigh sysmac(mac) %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
rem 3. First CP produces more lines than second
set TESTCMDFILE1=rt\test\bat\TESTCMD4C.BAT
set TESTCMDFILE2=rt\test\bat\TESTCMD4B.BAT
call bat\ASMLG %z_TraceMode% rt\test\TESTCMD4 notiming stats bal noloadhigh sysmac(mac) %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error

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