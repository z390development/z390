@echo off
rem demo hello world demo
setlocal
pushd %~dp0
cd..
call bat\ASMLG demo\HELLO %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASM demo\DEMOM8Q1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
set sysut1=demo\TESTDCB1.TF1
set sysut2=demo\TESTDCB1.TF2
set sysout=demo\TESTDCB1.TF3
call bat\ASMLG demo\TESTDCB1 %1 %2 %3 %4 %5 %6 %7 %8 %9  || goto error
type demo\TESTDCB1.TF2
@REM pause verify demo\TESTDCB1 copied TESTDCB1.TF1 to TESTDCB1.TF2
goto:eof

:error
echo RUNASMDE.ERROR: Encountered RC %ERRORLEVEL% - exiting
exit /b %ERRORLEVEL%
