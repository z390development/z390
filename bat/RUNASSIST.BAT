@if /I "%1" == "tron" (echo on) else (echo off)
rem run demo and test assist programs

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
set XPRNT=assist\demo\DEMOAST1.XPR
call bat\asmlg %z_TraceMode% assist\demo\DEMOAST1 ASSIST TRACEALL || goto error

set XREAD=assist\test\TESTAST1.XRD
set XPRNT=assist\test\TESTAST1.XPR
set XPNCH=assist\test\TESTAST1.XPH
set XGET=assist\test\TESTAST1.XGT
set XPUT=assist\test\TESTAST1.XPT
call bat\asmlg %z_TraceMode% assist\test\TESTAST1 ASSIST TRACEALL || goto error

set XPRNT=assist\test\TSTXDECI.XPR
call bat\asmlg %z_TraceMode% assist\test\TSTXDECI ASSIST PARM(v)  || goto error

set XPRNT=assist\test\TSTXHEXI.XPR
call bat\asmlg %z_TraceMode% assist\test\TSTXHEXI ASSIST PARM(v)  || goto error

set XPRNT=assist\test\SOLP06.XPR
call bat\asmlg %z_TraceMode% assist\test\SOLP06 ASSIST PARM(v)    || goto error
call fc.exe assist\test\SOLP06.XPR assist\test\SOLP06.TF1         || goto error2

set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% while creating %XPRNT% - exiting
goto return
:error2
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: created output in %XPRNT% does not match reference file - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
