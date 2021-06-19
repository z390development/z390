rem run demo and test assist programs
@setlocal
@pushd %~dp0
cd..

set XPRNT=assist\demo\DEMOAST1.XPR
call bat\asmlg assist\demo\DEMOAST1 ASSIST TRACEALL
IF %ERRORLEVEL% NEQ 0 (
    echo RUNASSIS.ERROR: ERROR IN DEMOAST1 RC=%ERRORLEVEL% - exiting
    EXIT /B %ERRORLEVEL%
)

set XREAD=assist\test\TESTAST1.XRD
set XPRNT=assist\test\TESTAST1.XPR
set XPNCH=assist\test\TESTAST1.XPH
set XGET=assist\test\TESTAST1.XGT
set XPUT=assist\test\TESTAST1.XPT
call bat\asmlg assist\test\TESTAST1 ASSIST TRACEALL
IF %ERRORLEVEL% NEQ 0 (
    echo.RUNASSIS.ERROR: ERROR IN TESTAST1 RC=%ERRORLEVEL% - exiting
    EXIT /B %ERRORLEVEL%
)