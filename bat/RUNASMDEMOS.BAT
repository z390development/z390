@if /I "%1" == "tron" (echo on) else (echo off)
rem demo hello world demo

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
call bat\ASMLG %z_TraceMode% demo\HELLO    nocon %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASM   %z_TraceMode% demo\DEMOM8Q1 nocon %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% demo\DEMOBMK1 nocon %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% demo\DEMOBMK2 nocon %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% demo\DEMODFP1 nocon %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASM   %z_TraceMode% demo\DEMONUM1 nocon %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% demo\DEMONUM2 nocon %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% demo\DEMOSTR1 nocon %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% demo\DEMOWTO2 nocon %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% demo\SIEVE    nocon %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% demo\STDDEVLB nocon %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error

rem when running this script using a pre-built distribution, the line endings are Linux-type causing file compares to fail
rem we've disabled the file compares as of May 24, 2022. Issue 643 covers this case and requests a repair.

set sysut1=demo\TESTDCB1.TF1
set sysut2=demo\TESTDCB1.TF2
set sysout=demo\TESTDCB1.TF3
call bat\ASMLG %z_TraceMode% demo\TESTDCB1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error

REM Normalize line endings for comparison (remove CRs) using only batch commands
call findstr.exe /V /R "" demo\TESTDCB1.TF1 > "%TEMP%\dcb1_1.tmp" 2>nul
call findstr.exe /V /R "" demo\TESTDCB1.TF2 > "%TEMP%\dcb1_2.tmp" 2>nul
call comp.exe "%TEMP%\dcb1_1.tmp" "%TEMP%\dcb1_2.tmp" /A /M || goto error_dcb1

set sysut1=demo\TESTDCB2.TF1
set sysut2=demo\TESTDCB2.TF2
set sysout=demo\TESTDCB2.TF3
call bat\ASMLG %z_TraceMode% demo\TESTDCB2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error

REM Normalize line endings for comparison (remove CRs) using only batch commands
call findstr.exe /V /R "" demo\TESTDCB2.TF1 > "%TEMP%\dcb2_1.tmp" 2>nul
call findstr.exe /V /R "" demo\TESTDCB2.TF2 > "%TEMP%\dcb2_2.tmp" 2>nul
call comp.exe "%TEMP%\dcb2_1.tmp" "%TEMP%\dcb2_2.tmp" /A /M || goto error_dcb2

set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
goto return
:error_dcb1
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: TESTDCB1 data were not copied correctly
goto return
:error_dcb2
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: TESTDCB2 data were not copied correctly
goto return
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
