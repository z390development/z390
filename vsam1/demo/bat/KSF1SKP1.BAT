@echo off
REM READ KSDS KSF1NAME STARTING AT KEY >= "MQ" (SKIP MM, GET MT NEXT)
rem %0 in z390\vsam1\demo\bat; switch to z390
pushd %~dps0..\..\..

SET VSAMPATH=vsam1\demo
SET ZVSAM=ZVSAM(1)
SET TRACE=%
IF /I "%1" == "traceall" (
    SET TRACE=TRACEALL
    shift /1
)

SET RC=0

SET VFILE=%VSAMPATH%\DEMOCAT.KSF1NAME
CALL bat\ASMLG %VSAMPATH%\KSF1SKP1 %1 %2 %3 %4 %5 %ZVSAM% %TRACE%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ERROR IN POINT, GET KSF1NAME
	GOTO errmsg
)

:return
popd
exit /b %RC%
:errmsg
echo ERROR: %ERRMSG%
SET RC=8
GOTO return