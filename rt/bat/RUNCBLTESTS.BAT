@if /I "%1" == "tron" (echo on) else (echo off)
rem run z390 zcobol sample regression tests

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
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTADD1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTADD2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
rem TESTCAL3 statically calls TESTASM4 - link them together
call bat\ASM    %z_TraceMode% zcobol\tests\TESTASM4 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCL  %z_TraceMode% zcobol\tests\TESTCAL3 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
rem TESTCAL1 statically calls TESTCAL2 and dynamically calls TESTCAL3
call bat\CBLC   %z_TraceMode% zcobol\tests\TESTCAL2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTCAL1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTCMP1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTCMP2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTCMP3 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTCMP4 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTCMP5 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTCMP6 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTCPY1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTCPY2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTDFP1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTDIV1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTDIV2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTDSP1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
set INFILE=zcobol\tests\TESTFIL1.IN
set OUTFILE=zcobol\tests\TESTFIL1.OUT
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTFIL1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
set INFILE=ZCOBOL\TESTS\TESTFIL2.IN
set OUTFILE=ZCOBOL\TESTS\TESTFIL2.OUT
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTFIL2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
set INFILE=
set OUTFILE=
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTGO1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTHFP1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTISP1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTPIC1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTPM1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTPM2  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
set INFILE=zcobol\tests\TESTPM3.IN
set OUTFILE=zcobol\tests\TESTPM3.OUT
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTPM3  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
set INFILE=
set OUTFILE=
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTSIX1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTSIX2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTSUB1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTSUB2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTTRC1 %1 %2 %3 %4 %5 %6 %7 %8 %9 TRUNC       || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTTRC2 %1 %2 %3 %4 %5 %6 %7 %8 %9             || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTTRC3 %1 %2 %3 %4 %5 %6 %7 %8 %9 TRUNC NOR64 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTWS1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error

call bat\CBLCLG %z_TraceMode% zcobol\tests\testbfp1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\testif1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\testif2  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\testif3  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\testmov1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLClG %z_TraceMode% zcobol\tests\testmov2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\testmov3 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\CM101M01 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\CM101M02 %1 %2 %3 %4 %5 %6 %7 %8 %9
     if %ERRORLEVEL% NEQ 8 goto error
call bat\CBLC   %z_TraceMode% zcobol\tests\CM101M03 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLC   %z_TraceMode% zcobol\tests\CM401M01 %1 %2 %3 %4 %5 %6 %7 %8 %9
     if %ERRORLEVEL% NEQ 8 goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\DB101A01 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\NC106A01 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\TESTREPL %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\CBLCLG %z_TraceMode% zcobol\tests\YOUZPI1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
set INFILE=zcobol\tests\SORTASC.TF1
set OUTFILE=zcobol\tests\SORTASC.OUT
call bat\CBLCLG %z_TraceMode% zcobol\tests\SORTASC  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc zcobol\tests\SORTASC.OUT zcobol\tests\SORTASC.TF2 > nul || goto error
set INFILE=zcobol\tests\SORTDEF.TF1
set OUTFILE=zcobol\tests\SORTDEF.OUT
call bat\CBLCLG %z_TraceMode% zcobol\tests\SORTDEF  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc zcobol\tests\SORTDEF.OUT zcobol\tests\SORTDEF.TF2 > nul || goto error
set INFILE=zcobol\tests\SORTPROC.TF1
set OUTFILE=zcobol\tests\SORTPROC.OUT
call bat\CBLCLG %z_TraceMode% zcobol\tests\SORTPROC %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc zcobol\tests\SORTPROC.OUT zcobol\tests\SORTPROC.TF2 > nul || goto error
set INFILE=zcobol\tests\SORTMULT.TF1
set ASCFILE=zcobol\tests\SORTMULT_ASC.OUT
set DESCFILE=zcobol\tests\SORTMULT_DSC.OUT
call bat\CBLCLG %z_TraceMode% zcobol\tests\SORTMULT %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc zcobol\tests\SORTMULT_ASC.OUT zcobol\tests\SORTMULT_ASC.TF2 > nul || goto error
fc zcobol\tests\SORTMULT_DSC.OUT zcobol\tests\SORTMULT_DSC.TF2 > nul || goto error
set INFILE=
set OUTFILE=
set ASCFILE=
set DESCFILE=
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
