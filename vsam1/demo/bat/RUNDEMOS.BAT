@echo off
REM ASSEMBLE, LINK, AND EXEC DEMOS

rem %0 in z390\vsam1\demo\bat; switch to z390
pushd %~dps0..\..\..

SET VSAMPATH=vsam1\demo
SET ZVSAM=ZVSAM(1)

SET RC=0

CALL %VSAMPATH%\bat\DEMOCAT %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=DEMOCAT unable to build DEMOCAT.390
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESF1DEMO %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESF1DEMO failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESV1DEMO %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESV1DEMO failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\RRF1DEMO %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=RRF1DEMO failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\RRV1DEMO %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=RRV1DEMO failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\KSF1RPO1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=KSF1RPO1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\KSF1DEMO %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=KSF1DEMO failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\KSF1SKP1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=KSF1SKP1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\KSF1RPO2 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=KSF1RPO2 failed
    GOTO errmsg
)

ECHO All demos ran successfully

:return
popd
exit /b %RC%
:errmsg
echo ERROR: %ERRMSG%
SET RC=8
GOTO return               
