@echo off
REM RETRIEVE INSERTED DSH1,DSH2,DSH3 BY KEY AND SEQ FWD AND BWD
rem %0 in z390\vsam1\test\bat; switch to z390
pushd %~dps0..\..\..

SET VSAMPATH=vsam1\test
SET ZVSAM=ZVSAM(1)
SET TRACE=%
IF /I "%1" == "traceall" (
    SET TRACE=TRACEALL
    shift /1
)

SET RC=0

SET VFILE=%VSAMPATH%\TESTCAT.KSF2NAME
CALL bat\ASMLG %VSAMPATH%\KSF2TST3 %1 %2 %3 %4 %5 %ZVSAM% %TRACE%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ERROR IN KSF2TST3
	GOTO errmsg
)

:return
popd
exit /b %RC%
:errmsg
echo ERROR: %ERRMSG%
SET RC=8
GOTO return
