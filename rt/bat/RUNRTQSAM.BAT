@if /I "%1" == "tron" (echo on) else (echo off)
rem RUN RT QSAM FOR V,VB,VT,VL
rem BLD? CREATES TEST FILE, CHK? VERIFIES TEST FILE
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

pushd %~dp0\..\..
erase QSAM\BLD*.ERR
erase QSAM\TEST*.TFV
set SNAPOUT=DUMMY
set SYSUT2=QSAM\TESTV.TFV
call bat\ASMLG %z_TraceMode% QSAM\BLDV.MLC   TRACE %1 || goto error
set SYSUT1=QSAM\TESTV.TFV
call bat\ASMLG %z_TraceMode% QSAM\CHKV.MLC   TRACE    || goto error
set SYSUT2=QSAM\TESTVB.TFV
call bat\ASMLG %z_TraceMode% QSAM\BLDVB.MLC  TRACE %1 || goto error
set SYSUT1=QSAM\TESTVB.TFV
call bat\ASMLG %z_TraceMode% QSAM\CHKVB.MLC  TRACE    || goto error
set SYSUT2=QSAM\TESTVT.TFV
call bat\ASMLG %z_TraceMode% QSAM\BLDVT.MLC  TRACE %1 || goto error
set SYSUT1=QSAM\TESTVT.TFV
call bat\ASMLG %z_TraceMode% QSAM\CHKVT.MLC  TRACE    || goto error
set SYSUT2=QSAM\TESTVL.TFV
call bat\ASMLG %z_TraceMode% QSAM\BLDVL.MLC  TRACE %1 || goto error
set SYSUT1=QSAM\TESTVL.TFV
call bat\ASMLG %z_TraceMode% QSAM\CHKVL.MLC  TRACE    || goto error
set SYSUT2=QSAM\TESTVBL.TFV
call bat\ASMLG %z_TraceMode% QSAM\BLDVBL.MLC TRACE %1 || goto error
set SYSUT1=QSAM\TESTVBL.TFV
call bat\ASMLG %z_TraceMode% QSAM\CHKVBL.MLC TRACE    || goto error
set SYSUT2=QSAM\TESTVTL.TFV
call bat\ASMLG %z_TraceMode% QSAM\BLDVTL.MLC TRACE %1 || goto error
set SYSUT1=QSAM\TESTVTL.TFV
call bat\ASMLG %z_TraceMode% QSAM\CHKVTL.MLC TRACE    || goto error
set SYSUT2=QSAM\TESTF.TFV
call bat\ASMLG %z_TraceMode% QSAM\BLDF.MLC   TRACE %1 || goto error
set SYSUT1=QSAM\TESTF.TFV
call bat\ASMLG %z_TraceMode% QSAM\CHKF.MLC   TRACE    || goto error
set SYSUT2=QSAM\TESTFL.TFV
call bat\ASMLG %z_TraceMode% QSAM\BLDFL.MLC  TRACE %1 || goto error
set SYSUT1=QSAM\TESTFL.TFV
call bat\ASMLG %z_TraceMode% QSAM\CHKFL.MLC  TRACE    || goto error
set SYSUT2=QSAM\TESTFT.TFV
call bat\ASMLG %z_TraceMode% QSAM\BLDFT.MLC  TRACE %1 || goto error
set SYSUT1=QSAM\TESTFT.TFV
call bat\ASMLG %z_TraceMode% QSAM\CHKFT.MLC  TRACE    || goto error
set SYSUT2=QSAM\TESTFTL.TFV
call bat\ASMLG %z_TraceMode% QSAM\BLDFTL.MLC TRACE %1 || goto error
set SYSUT1=QSAM\TESTFTL.TFV
call bat\ASMLG %z_TraceMode% QSAM\CHKFTL.MLC TRACE    || goto error
dir QSAM\TEST*.TFV
set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
