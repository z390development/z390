@if /I "%1" == "tron" (echo on) else (echo off)
rem QSAM handling of file with recordtype=VB

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
REM RUN CHKVB TO VERIFY RECFM=VB TYPE TEST FILE VIA SYSUT1
ERASE QSAM\CHKVB.ERR
SET SYSUT1=QSAM\TESTVB.Binary.TFV
SET SNAPOUT=DUMMY
CALL BAT\ASMLG QSAM\CHKVB.MLC TRACEALL
DIR QSAM\TESTVB.Binary.TFV
PAUSE VERIFY CHKVB

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
