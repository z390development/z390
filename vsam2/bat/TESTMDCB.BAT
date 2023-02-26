@echo off
REM ASSEMBLE, LINK MODCB TEST
rem %0 in z390\vsam2\bat; switch to z390
pushd %~dps0..\..

SET VSAMPATH=vsam2
SET ZVSAM=ZVSAM(2)

SET RC=0
CALL bat\asml %VSAMPATH%\mlc\TESTMDCB %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ERROR IN TESTMDCB
	GOTO errmsg
)

:return
popd
exit /b %RC%
:errmsg
echo ERROR: %ERRMSG%
SET RC=8
GOTO return
