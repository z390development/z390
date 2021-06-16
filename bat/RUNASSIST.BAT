rem run demo and test assist programs
@setlocal
@pushd %~dp0
cd..
set XPRNT=assist\demo\DEMOAST1.XPR
call bat\asmlg assist\demo\DEMOAST1 ASSIST TRACEALL
IF ERRORLEVEL 1 @ECHO ERROR IN DEMOAST1 & EXIT \B 1
set XREAD=assist\test\TESTAST1.XRD
set XPRNT=assist\test\TESTAST1.XPR
set XPNCH=assist\test\TESTAST1.XPH
set XGET=assist\test\TESTAST1.XGT
set XPUT=assist\test\TESTAST1.XPT
call bat\asmlg assist\test\TESTAST1 ASSIST TRACEALL
IF ERRORLEVEL 1 @ECHO ERROR IN TESTAST1 & EXIT \B 1
@REM pause verify assist demo and test