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
call bat\ASMLG %z_TraceMode% demo\SIEVE    nocon %1 %2 %3 %4 %5 %6 %7 %8 %9 time(20) || goto error
rem test assembly from source dir
cd demo
call ..\bat\ASMLG %z_TraceMode%   STDDEVLB nocon %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
cd ..

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

REM demo usage of non-default filetypes
:DEMOEXT
if     exist demo\DEMOEXT.390      del  demo\DEMOEXT.390      >nul
if     exist demo\DEMOEXT.OBJ      del  demo\DEMOEXT.OBJ      >nul
if     exist demo\DEMOEXT.TRE      del  demo\DEMOEXT.TRE      >nul
if     exist demo\DEMOEXT.zBindOut del  demo\DEMOEXT.zBindOut >nul
if     exist demo\DEMOEXT.zErrors  del  demo\DEMOEXT.zErrors  >nul
if     exist demo\DEMOEXT.zExpand* del  demo\DEMOEXT.zExpand* >nul
if     exist demo\DEMOEXT.zListing del  demo\DEMOEXT.zListing >nul
if     exist demo\DEMOEXT.zLog     del  demo\DEMOEXT.zLog     >nul
if     exist demo\DEMOEXT.zLogFile del  demo\DEMOEXT.zLogFile >nul
if     exist demo\DEMOEXT.zLog.*   del  demo\DEMOEXT.zLog.*   >nul
if     exist demo\DEMOEXT.zPunch   del  demo\DEMOEXT.zPunch   >nul
if     exist demo\DEMOEXT.zStats   del  demo\DEMOEXT.zStats   >nul
if     exist demo\DEMOEXT.zTerm    del  demo\DEMOEXT.zTerm    >nul
rem call bat\mz390 %z_TraceMode% demo\DEMOEXT.zAsm @demo\DEMOEXT0.zOptions >nul || goto error
java -classpath %~dps0..\z390.jar -Xrs -Xms150000K -Xmx150000K %J2SEOPTIONS% mz390 DEMOEXT.zASM @demo\DEMOEXT0.zOptions || goto error
call bat\lz390 %z_TraceMode% demo\DEMOEXT.OBJ  @demo\DEMOEXT0.zOptions      || goto error
call bat\ez390 %z_TraceMode% demo\DEMOEXT      @demo\DEMOEXT1.zOptions      || goto error
if not exist demo\DEMOEXT.390      echo demo\DEMOEXT.390      was not created && goto error_file
if not exist demo\DEMOEXT.OBJ      echo demo\DEMOEXT.OBJ      was not created && goto error_file
if not exist demo\DEMOEXT.TRE      echo demo\DEMOEXT.TRE      was not created && goto error_file
if not exist demo\DEMOEXT.zBindOut echo demo\DEMOEXT.zBindOut was not created && goto error_file
rem -- no zErrors because there are no errors to be reported
if not exist demo\DEMOEXT.zExpande echo demo\DEMOEXT.zExpande was not created && goto error_file
if not exist demo\DEMOEXT.zListing echo demo\DEMOEXT.zListing was not created && goto error_file
if not exist demo\DEMOEXT.zLogFile echo demo\DEMOEXT.zLogFile was not created && goto error_file
if not exist demo\DEMOEXT.zLog.TRA echo demo\DEMOEXT.zLog.TRA was not created && goto error_file
if not exist demo\DEMOEXT.zLog.TRL echo demo\DEMOEXT.zLog.TRL was not created && goto error_file
if not exist demo\DEMOEXT.zLog.TRM echo demo\DEMOEXT.zLog.TRM was not created && goto error_file
if not exist demo\DEMOEXT.zPunch   echo demo\DEMOEXT.zPunch   was not created && goto error_file
if not exist demo\DEMOEXT.zStats   echo demo\DEMOEXT.zStats   was not created && goto error_file
if not exist demo\DEMOEXT.zTerm    echo demo\DEMOEXT.zTerm    was not created && goto error_file
if     exist demo\DEMOEXT.390      del  demo\DEMOEXT.390      >nul
if     exist demo\DEMOEXT.OBJ      del  demo\DEMOEXT.OBJ      >nul
if     exist demo\DEMOEXT.TRE      del  demo\DEMOEXT.TRE      >nul
if     exist demo\DEMOEXT.zBindOut del  demo\DEMOEXT.zBindOut >nul
if     exist demo\DEMOEXT.zErrors  del  demo\DEMOEXT.zErrors  >nul
if     exist demo\DEMOEXT.zExpand* del  demo\DEMOEXT.zExpand* >nul
if     exist demo\DEMOEXT.zListing del  demo\DEMOEXT.zListing >nul
if     exist demo\DEMOEXT.zLog     del  demo\DEMOEXT.zLog     >nul
if     exist demo\DEMOEXT.zLogFile del  demo\DEMOEXT.zLogFile >nul
if     exist demo\DEMOEXT.zLog.*   del  demo\DEMOEXT.zLog.*   >nul
if     exist demo\DEMOEXT.zPunch   del  demo\DEMOEXT.zPunch   >nul
if     exist demo\DEMOEXT.zStats   del  demo\DEMOEXT.zStats   >nul
if     exist demo\DEMOEXT.zTerm    del  demo\DEMOEXT.zTerm    >nul
echo Demo of non-default extensions completed okay
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
:error_file
set z_ReturnCode=8
goto return
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
