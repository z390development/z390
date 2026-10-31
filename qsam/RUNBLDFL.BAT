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
REM RUN BLDFL TO CREATE RECFM=F WITH LARGE BLOCKS
ERASE QSAM\BLDFL.ERR
ERASE QSAM\TESTFL.EBCDIC.TFV
SET SYSUT2=QSAM\TESTFL.EBCDIC.TFV
SET SNAPOUT=DUMMY
CALL BAT\ASMLG QSAM\BLDFL.MLC TRACE
DIR QSAM\TESTFL.EBCDIC.TFV
PAUSE VERIFY BLDFL

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
