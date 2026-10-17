@if /I "%1" == "tron" (echo on) else (echo off)
rem QSAM handling of file with recordtype=VBL

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
REM RUN CHKVBL TO VERIFY RECFM=VB WITH LBI BLK=100008
ERASE QSAM\CHKVBL.ERR
SET SYSUT1=QSAM\TESTVBL.Binary.TFV
SET SNAPOUT=DUMMY
CALL BAT\ASMLG QSAM\CHKVBL.MLC TRACEALL DUMP
DIR QSAM\TESTVBL.Binary.TFV
PAUSE VERIFY CHKVBL

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
