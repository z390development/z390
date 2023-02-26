@echo off
REM ASSEMBLE, LINK ZVSAM19C ZVSAM(2)
rem %0 in z390\vsam2\bat; switch to z390
pushd %~dps0..\..

SET VSAMPATH=vsam2
SET ZVSAM=ZVSAM(2)

SET RC=0
CALL bat\asml %VSAMPATH%\mlc\ZVSAM19C %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ERROR IN ZVSAM19C
	GOTO errmsg
)

:return
popd
exit /b %RC%
:errmsg
echo ERROR: %ERRMSG%
SET RC=8
GOTO return
