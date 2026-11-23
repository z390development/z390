@if /I "%1" == "tron" (echo on) else (echo off)
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
REM RUN CHKF TO VERIFY RECFM=F TYPE TEST FILE VIA SYSUT2
ERASE QSAM\CHKF.ERR
SET SYSUT1=QSAM\TESTF.EBCDIC.TFV
SET SNAPOUT=DUMMY
CALL BAT\ASMLG QSAM\CHKF.MLC TRACEALL
DIR QSAM\TESTF.EBCDIC.TFV
PAUSE VERIFY CHKF

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
