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
call bat\ASMLG %z_TraceMode% rt\test\TESTCPY1 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% rt\test\TESTCPY2 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error

rem TESTCTD1 writes WTO lines we need to compare against reference file TESTCTD1.TF1
call bat\ASMLG %z_TraceMode% rt\test\TESTCTD1 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
findstr /v /c:"EZ390" /c:"ABSOLUTELY NO WARRANTY" /c:"This is free software" /c:"under certain conditions" /c:"SYSMAC(" rt\test\TESTCTD1.LOG > rt\test\TESTCTD1.TMP
fc rt\test\TESTCTD1.TMP rt\test\TESTCTD1.TF1 > nul || goto error
del rt\test\TESTCTD1.TMP

call bat\ASMLG %z_TraceMode% rt\test\TESTCVB1 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% rt\test\TESTCVBG notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error

rem  bat\ASMLG %z_TraceMode% rt\test\TESTDCB1 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error -- moved to demo
set sysut1=rt\test\TESTDCB2.TF1
set sysut2=rt\test\TESTDCB2.OUT
set sysout=rt\test\TESTDCB2.RPT
call bat\asmlg %z_TraceMode% rt\test\TESTDCB2 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc rt\test\TESTDCB2.OUT rt\test\TESTDCB2.TF2 > nul || goto error
fc rt\test\TESTDCB2.RPT rt\test\TESTDCB2.TF3 > nul || goto error
set sysut1=rt\test\TESTDCB3.TF1
set sysut2=rt\test\TESTDCB3.OUT
set sysout=rt\test\TESTDCB3.RPT
call bat\asmlg %z_TraceMode% rt\test\TESTDCB3 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc rt\test\TESTDCB3.OUT rt\test\TESTDCB3.TF1 > nul || goto error
fc rt\test\TESTDCB3.RPT rt\test\TESTDCB3.TF3 > nul || goto error
set sysut1=rt\test\TESTDCB4.TF1
set sysut2=rt\test\TESTDCB4.OUT
set sysout=rt\test\TESTDCB4.RPT
call bat\asmlg %z_TraceMode% rt\test\TESTDCB4 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc rt\test\TESTDCB4.OUT rt\test\TESTDCB4.TF1 > nul || goto error
fc rt\test\TESTDCB4.RPT rt\test\TESTDCB4.TF3 > nul || goto error
set sysut1=rt\test\TESTDCB5.TF1
set sysut2=rt\test\TESTDCB5.OUT
set sysout=rt\test\TESTDCB5.RPT
call bat\asmlg %z_TraceMode% rt\test\TESTDCB5 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc rt\test\TESTDCB5.OUT rt\test\TESTDCB5.TF2 > nul || goto error
fc rt\test\TESTDCB5.RPT rt\test\TESTDCB5.TF3 > nul || goto error
set sysut1=rt\test\TESTDCB6.TF1
set sysut2=rt\test\TESTDCB6.OUT
set sysout=rt\test\TESTDCB6.RPT
call bat\asmlg %z_TraceMode% rt\test\TESTDCB6 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc rt\test\TESTDCB6.OUT rt\test\TESTDCB6.TF2 > nul || goto error
fc rt\test\TESTDCB6.RPT rt\test\TESTDCB6.TF3 > nul || goto error
set sysut1=rt\test\TESTDCB7.TF1
set sysut2=rt\test\TESTDCB7.OUT
set sysout=rt\test\TESTDCB7.RPT
call bat\asmlg %z_TraceMode% rt\test\TESTDCB7 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc rt\test\TESTDCB7.OUT rt\test\TESTDCB7.TF2 > nul || goto error
fc rt\test\TESTDCB7.RPT rt\test\TESTDCB7.TF3 > nul || goto error
set sysut1=rt\test\TESTDCB8.TF1
set sysut2=rt\test\TESTDCB8.OUT
set sysout=rt\test\TESTDCB8.RPT
call bat\asmlg %z_TraceMode% rt\test\TESTDCB8 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc rt\test\TESTDCB8.OUT rt\test\TESTDCB8.TF1 > nul || goto error
fc rt\test\TESTDCB8.RPT rt\test\TESTDCB8.TF3 > nul || goto error
set sysut1=rt\test\TESTDCB9.TF1
set sysut2=rt\test\TESTDCB9.OUT
set sysout=rt\test\TESTDCB9.RPT
call bat\asmlg %z_TraceMode% rt\test\TESTDCB9 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc rt\test\TESTDCB9.OUT rt\test\TESTDCB9.TF2 > nul || goto error
fc rt\test\TESTDCB9.RPT rt\test\TESTDCB9.TF3 > nul || goto error
set sysut1=
set sysut2=
set sysout=
call bat\asmlg %z_TraceMode% rt\test\TESTDCBA notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc rt\test\TESTDCBA.OUT rt\test\TESTDCBA.TF1 > nul || goto error
fc rt\test\TESTDCBA.RPT rt\test\TESTDCBA.TF3 > nul || goto error
set sysut1=rt\test\TESTDCBB.TF1
set sysut2=rt\test\TESTDCBB.OUT
set sysout=rt\test\TESTDCBB.RPT
call bat\asmlg %z_TraceMode% rt\test\TESTDCBB notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc rt\test\TESTDCBB.OUT rt\test\TESTDCBB.TF1 > nul || goto error
fc rt\test\TESTDCBB.RPT rt\test\TESTDCBB.TF3 > nul || goto error
set sysut1=
set sysut2=
set sysout=
call bat\asmlg %z_TraceMode% rt\test\TESTDCBC notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc rt\test\TESTDCBC.OUT rt\test\TESTDCBC.TF1 > nul || goto error
fc rt\test\TESTDCBC.RPT rt\test\TESTDCBC.TF3 > nul || goto error
set sysut1=
set sysut2=
set sysout=
call bat\asmlg %z_TraceMode% rt\test\TESTDCBD notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc rt\test\TESTDCBD.OUT rt\test\TESTDCBD.TF2 > nul || goto error
fc rt\test\TESTDCBD.RPT rt\test\TESTDCBD.TF3 > nul || goto error
set sysut1=
set sysut2=
set sysout=
call bat\asmlg %z_TraceMode% rt\test\TESTDCBE notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc rt\test\TESTDCBE.OUT rt\test\TESTDCBE.TF2 > nul || goto error
fc rt\test\TESTDCBE.RPT rt\test\TESTDCBE.TF3 > nul || goto error
set sysut1=rt\test\TESTDCBF.TF1
set sysut2=rt\test\TESTDCBF.OUT
set sysout=rt\test\TESTDCBF.RPT
call bat\asmlg %z_TraceMode% rt\test\TESTDCBF notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc rt\test\TESTDCBF.OUT rt\test\TESTDCBF.TF2 > nul || goto error
fc rt\test\TESTDCBF.RPT rt\test\TESTDCBF.TF3 > nul || goto error






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
