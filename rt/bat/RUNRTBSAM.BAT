@if /I "%1" == "tron" (echo on) else (echo off)
rem RUN RT BSAM	READ AND WRITE WITH/WITHOUT LBI
rem BLD? CREATES TEST FILE, CHK? VERIFIES TEST FILE, CPY? COPIES FILE
rem SEE TRE TRACE FILES FOR SNAP DUMPS OF RECORDS

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
erase bsam\*.ERR
erase bsam\TEST*.TFV
set SNAPOUT=DUMMY
set SYSUT2=bsam\TESTW.TFV
call bat\ASMLG %z_TraceMode% bsam\BLDW.MLC TRACE   || goto error
set SYSUT1=bsam\TESTW.TFV
call bat\ASMLG %z_TraceMode% bsam\CHKW.MLC TRACE   || goto error
set SYSUT2=bsam\TESTWL.TFV
call bat\ASMLG %z_TraceMode% bsam\BLDWL.MLC TRACE  || goto error
set SYSUT1=bsam\TESTWL.TFV
call bat\ASMLG %z_TraceMode% bsam\CHKWL.MLC TRACE  || goto error
set SYSUT1=bsam\TESTW.TFV
set SYSUT2=bsam\CPYRW.VES
call bat\ASMLG %z_TraceMode% bsam\CPYRW.MLC TRACE  || goto error
set SYSUT1=bsam\TESTwL.TFV
set SYSUT2=bsam\CPYRWL.VES
call bat\ASMLG %z_TraceMode% bsam\CPYRWL.MLC TRACE || goto error
dir bsam\TEST*.TFV
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
