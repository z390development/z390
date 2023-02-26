@echo off
REM WRITE AND READ RRDS VAR LENGTH RECORDS BY REL RCD #
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

CALL bat\ASML %VSAMPATH%\DEMOCAT %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ERROR: unable to build DEMOCAT.390 VSAM catalog
	GOTO errmsg
)
SET VFILE=%VSAMPATH%\DEMOCAT.RRF1
CALL bat\ASMLG %VSAMPATH%\RRF1DEMO %1 %2 %3 %4 %5  %ZVSAM% %TRACE%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ERROR IN RRF1DEMO
	GOTO errmsg
)

:return
popd
exit /b %RC%
:errmsg
echo ERROR: %ERRMSG%
SET RC=8
GOTO return
