@if /I "%1" == "tron" (echo on) else (echo off)
rem regression test scripts
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
rem -- test generated Q command when EOF is reached
set  rtscript=rt\rt\BATCH.RT
call bat\asmlg %z_TraceMode% demo\hello      sysmac(mac) noloadhigh test(rtscript)
     if %ERRORLEVEL% NEQ 16 goto error
echo script ended RC=16 as expected

rem -- test normal termination when z command is executed
set  rtscript=rt\rt\BATCHZ.RT   && call bat\asmlg %z_TraceMode% demo\hello     sysmac(mac) noloadhigh test(rtscript)                     || goto error

rem -- other test scripts
set  rtscript=rt\rt\OPCD_DOS.RT && call bat\asmlg %z_TraceMode% rt\rt\OPCD_DOS sysmac(mac) noloadhigh test(rtscript) @rt\rt\OPCD_DOS.OPT || goto error

call bat\asm   %z_TraceMode% rt\rt\OPCD_ERR  sysmac(mac) noloadhigh                @rt\rt\OPCD_ERR.OPT
     if %ERRORLEVEL% NEQ 12 goto error
rem Compare AZ390 diagnostic lines only (filter out MZ390 framing + FID= path)
findstr /v /c:"MZ390" /c:"FID=" rt\rt\OPCD_ERR.ERR > rt\rt\OPCD_ERR.ERR.flt
fc  rt\rt\OPCD_ERR.ERR.flt rt\rt\OPCD_ERR.TF1 > nul || goto error
del rt\rt\OPCD_ERR.ERR.flt
call bat\asml  %z_TraceMode% rt\rt\OPCD_UNI  sysmac(mac) noloadhigh @rt\rt\OPCD_UNI.OPT || goto error

set  rtscript=rt\rt\OPCD_XA.RT  && call bat\asmlg %z_TraceMode% rt\rt\OPCD_XA   sysmac(mac) noloadhigh test(rtscript) @rt\rt\OPCD_XA.OPT  || goto error

set  rtscript=rt\rt\OPCD_ZZZ.RT && call bat\asmlg %z_TraceMode% rt\rt\OPCD_ZZZ  sysmac(mac) noloadhigh                @rt\rt\OPCD_ZZZ.OPT || goto error

set  rtscript=rt\rt\RPI1540.RT  && call bat\asmlg %z_TraceMode% rt\rt\RPI1540   sysmac(mac) noloadhigh test(rtscript) || goto error
set  rtscript=rt\rt\RPI1544.RT  && call bat\asmlg %z_TraceMode% rt\rt\RPI1544   sysmac(mac) noloadhigh test(rtscript) || goto error

call bat\asmlg %z_TraceMode% rt\rt\RPI1609   sysmac(mac) noloadhigh
     if %ERRORLEVEL% NEQ 12 goto error

call bat\asmlg %z_TraceMode% rt\rt\RPI1625   sysmac(mac) noloadhigh
     if %ERRORLEVEL% NEQ 12 goto error

call bat\asmlg %z_TraceMode% rt\rt\RPI1641   sysmac(mac) noloadhigh zvsam(2)                  || goto error

call bat\asml  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) || goto error
call bat\asml  %z_TraceMode% rt\rt\RPI2009B  sysmac(mac) || goto error
call bat\asml  %z_TraceMode% rt\rt\RPI2009C  sysmac(mac) || goto error
call bat\asml  %z_TraceMode% rt\rt\RPI2009D  sysmac(mac) || goto error
call bat\asml  %z_TraceMode% rt\rt\RPI2009E  sysmac(mac) || goto error
call bat\asml  %z_TraceMode% rt\rt\RPI2009F  sysmac(mac) || goto error

set  rtscript=rt\rt\RPI2000.RT        && call bat\exec  %z_TraceMode% rt\rt\RPI2000   sysmac(mac) noloadhigh             test(rtscript) || goto error
set  rtscript=rt\rt\RPI2001A.RT       && call bat\exec  %z_TraceMode% rt\rt\RPI2001A  sysmac(mac) noloadhigh             test(rtscript) || goto error
set  rtscript=rt\rt\RPI2001B.RT       && call bat\exec  %z_TraceMode% rt\rt\RPI2001B  sysmac(mac) noloadhigh             test(rtscript) || goto error
set  rtscript=rt\rt\RPI2002A.RT       && call bat\exec  %z_TraceMode% rt\rt\RPI2002A  sysmac(mac) noloadhigh             test(rtscript) || goto error
set  rtscript=rt\rt\RPI2002B.RT       && call bat\exec  %z_TraceMode% rt\rt\RPI2002B  sysmac(mac) noloadhigh             test(rtscript) || goto error
set  rtscript=rt\rt\RPI2002C.RT       && call bat\exec  %z_TraceMode% rt\rt\RPI2002C  sysmac(mac) noloadhigh             test(rtscript) || goto error
set  rtscript=rt\rt\RPI2003.RT        && call bat\exec  %z_TraceMode% rt\rt\RPI2003   sysmac(mac) noloadhigh             test(rtscript) || goto error
set  rtscript=rt\rt\RPI2004.RT        && call bat\exec  %z_TraceMode% rt\rt\RPI2004   sysmac(mac) noloadhigh             test(rtscript) || goto error
set  rtscript=rt\rt\RPI2005.RT        && call bat\exec  %z_TraceMode% rt\rt\RPI2005   sysmac(mac) noloadhigh             test(rtscript) || goto error
set  rtscript=rt\rt\RPI2006.RT        && call bat\exec  %z_TraceMode% rt\rt\RPI2006   sysmac(mac) noloadhigh mem(32)     test(rtscript) || goto error
set  rtscript=rt\rt\RPI2007.RT        && call bat\exec  %z_TraceMode% rt\rt\RPI2007   sysmac(mac) noloadhigh             test(rtscript) || goto error
set  rtscript=rt\rt\RPI2008.RT        && call bat\exec  %z_TraceMode% rt\rt\RPI2008   sysmac(mac) noloadhigh             test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_A.RT     && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(A)     test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AFI.RT   && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AFI)   test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AG.RT    && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AG)    test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AGF.RT   && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AGF)   test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AGFI.RT  && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AGFI)  test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AGFR.RT  && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AGFR)  test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AGHI.RT  && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AGHI)  test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AGHIK.RT && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AGHIK) test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AGR.RT   && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AGR)   test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AGRK.RT  && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AGRK)  test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AGSI.RT  && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AGSI)  test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AH.RT    && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AH)    test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AHHHR.RT && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AHHHR) test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AHHLR.RT && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AHHLR) test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AHI.RT   && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AHI)   test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AHIK.RT  && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AHIK)  test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AHY.RT   && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AHY)   test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AIH.RT   && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AIH)   test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AR.RT    && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AR)    test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_ARK.RT   && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(ARK)   test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_ASI.RT   && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(ASI)   test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_AY.RT    && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(AY)    test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_LAA.RT   && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(LAA)   test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009A_LAAG.RT  && call bat\exec  %z_TraceMode% rt\rt\RPI2009A  sysmac(mac) noloadhigh parm(LAAG)  test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009B_S.RT     && call bat\exec  %z_TraceMode% rt\rt\RPI2009B  sysmac(mac) noloadhigh parm(S)     test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009B_SG.RT    && call bat\exec  %z_TraceMode% rt\rt\RPI2009B  sysmac(mac) noloadhigh parm(SG)    test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009B_SGF.RT   && call bat\exec  %z_TraceMode% rt\rt\RPI2009B  sysmac(mac) noloadhigh parm(SGF)   test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009B_SGFR.RT  && call bat\exec  %z_TraceMode% rt\rt\RPI2009B  sysmac(mac) noloadhigh parm(SGFR)  test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009B_SGR.RT   && call bat\exec  %z_TraceMode% rt\rt\RPI2009B  sysmac(mac) noloadhigh parm(SGR)   test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009B_SGRK.RT  && call bat\exec  %z_TraceMode% rt\rt\RPI2009B  sysmac(mac) noloadhigh parm(SGRK)  test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009B_SH.RT    && call bat\exec  %z_TraceMode% rt\rt\RPI2009B  sysmac(mac) noloadhigh parm(SH)    test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009B_SHHHR.RT && call bat\exec  %z_TraceMode% rt\rt\RPI2009B  sysmac(mac) noloadhigh parm(SHHHR) test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009B_SHHLR.RT && call bat\exec  %z_TraceMode% rt\rt\RPI2009B  sysmac(mac) noloadhigh parm(SHHLR) test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009B_SHY.RT   && call bat\exec  %z_TraceMode% rt\rt\RPI2009B  sysmac(mac) noloadhigh parm(SHY)   test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009B_SR.RT    && call bat\exec  %z_TraceMode% rt\rt\RPI2009B  sysmac(mac) noloadhigh parm(SR)    test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009B_SRK.RT   && call bat\exec  %z_TraceMode% rt\rt\RPI2009B  sysmac(mac) noloadhigh parm(SRK)   test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009B_SY.RT    && call bat\exec  %z_TraceMode% rt\rt\RPI2009B  sysmac(mac) noloadhigh parm(SY)    test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009C.RT       && call bat\exec  %z_TraceMode% rt\rt\RPI2009C  sysmac(mac) noloadhigh             test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009D.RT       && call bat\exec  %z_TraceMode% rt\rt\RPI2009D  sysmac(mac) noloadhigh             test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009E.RT       && call bat\exec  %z_TraceMode% rt\rt\RPI2009E  sysmac(mac) noloadhigh             test(rtscript) || goto error
set  rtscript=rt\rt\RPI2009F.RT       && call bat\exec  %z_TraceMode% rt\rt\RPI2009F  sysmac(mac) noloadhigh             test(rtscript) || goto error
set  rtscript=rt\rt\RPI2010.RT        && call bat\asmlg %z_TraceMode% rt\rt\RPI2010   sysmac(mac) noloadhigh             test(rtscript) || goto error

rem -- all checks passed without issue
set z_ReturnCode=0
goto return

:error
if %ERRORLEVEL% EQU 0 (set z_ReturnCode=0
               ) else (set z_ReturnCode=%ERRORLEVEL%)
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
