rem run demo and test assist programs
cd..
set XPRNT=assist\demo\DEMOAST1.XPR
call bat\asmlg assist\demo\DEMOAST1 ASSIST TRACEALL
IF ERRORLEVEL 1 PAUSE ERROR IN DEMOAST1
set XREAD=assist\test\TESTAST1.XRD
set XPRNT=assist\test\TESTAST1.XPR
set XPNCH=assist\test\TESTAST1.XPH
set XGET=assist\test\TESTAST1.XGT
set XPUT=assist\test\TESTAST1.XPT
call bat\asmlg assist\test\TESTAST1 ASSIST TRACEALL
IF ERRORLEVEL 1 PAUSE ERROR IN TESTAST1
pause verify assist demo and test