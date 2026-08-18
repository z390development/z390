@echo off
REM ASSEMBLE, LINK AND RUN ACB/RPL MAPPING TEST
rem %0 in z390\vsam2\bat; switch to z390
pushd %~dps0..\..

SET VSAMPATH=vsam2
SET ZVSAM=ZVSAM(2)

SET RC=0
CALL bat\asmlg %VSAMPATH%\mlc\TESTMPCB %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ERROR IN TESTMPCB
	GOTO errmsg
)

:return
popd
exit /b %RC%
:errmsg
echo ERROR: %ERRMSG%
SET RC=8
GOTO return
