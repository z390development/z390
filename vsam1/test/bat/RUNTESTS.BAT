@echo off
REM ASSEMBLE, LINK, AND EXEC TESTS

rem %0 in z390\vsam1\test\bat; switch to z390
pushd %~dps0..\..\..

SET VSAMPATH=vsam1\test
SET ZVSAM=ZVSAM(1)

SET RC=0

REM ************************************
REM build TESTCAT
REM ************************************
CALL %VSAMPATH%\bat\TESTCAT %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=TESTCAT unable to build TESTCAT.390
    GOTO errmsg
)
REM ************************************
REM ESFx tests
REM ************************************
CALL %VSAMPATH%\bat\ESF1RPO1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESF1RPO1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESF1RPQ1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESF1RPQ1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESF1ERR1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESF1ERR1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESF1SKP1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESF1SKP1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESF1TEST %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESF1TEST failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESF2TEST %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESF2TEST failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESF3GEN1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESF3GEN1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESF3TEST %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESF3TEST failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESF4RPO1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESF4RPO1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESF4RPQ1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESF4RPQ1 failed
    GOTO errmsg
)
REM ************************************
REM ESVx tests
REM ************************************
CALL %VSAMPATH%\bat\ESV1RPO1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESV1RPO1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESV1RPQ1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESV1RPQ1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESV1SKP1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESV1SKP1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESV1TEST %1 %2 %3 %4 %5 %ZVSAM%
IF %RC% NEQ 0 (
    SET ERRMSG=ESV1TEST failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESV2TEST %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESV2TEST failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESV3TEST %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESV3TEST failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESV4TEST %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESV4TEST failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESV5RPO1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESF4RPO1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\ESV5RPQ1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ESF4RPQ1 failed
    GOTO errmsg
)
REM ************************************
REM KSF1x tests
REM ************************************
CALL %VSAMPATH%\bat\KSF1RPO1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=KSF1RPO1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\KSF1TEST %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=KSF1TEST failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\KSF1AIXA %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=KSF1AIXA failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\KSF1AIXC %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=KSF1AIXC failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\KSF1AIXS %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=KSF1AIXS failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\KSF1AIXZ %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=KSF1AIXZ failed
    GOTO errmsg
)
REM ************************************
REM KSF2x tests for DSH
REM ************************************
CALL %VSAMPATH%\bat\KSF2REST %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=KSF2REST failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\KSF2TST1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=KSF2TST1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\KSF2TST2 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=KSF2TST2 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\KSF2TST3 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=KSF2TST3 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\KSF2TST4 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=KSF2TST4 failed
    GOTO errmsg
)
REM ************************************
REM RRFx tests
REM ************************************
CALL %VSAMPATH%\bat\RRF1RPO1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=RRF1RPO1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\RRF1SKP1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=RRF1SKP1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\RRF1TEST %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=RRF1TEST failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\RRF2RPO1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=RRF2RPO1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\RRF2RPQ1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=RRF2RPQ1 failed
    GOTO errmsg
)
REM ************************************
REM RRVx tests
REM ************************************
CALL %VSAMPATH%\bat\RRV1RPO1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=RRV1RPO1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\RRV1RPQ1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=RRV1RPQ1 failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\RRV1TEST %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=RRV1TEST failed
    GOTO errmsg
)
CALL %VSAMPATH%\bat\RRV1SKP1 %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=RRV1SKP1 failed
    GOTO errmsg
)
REM ************************************
REM Done
REM ************************************

ECHO All tests ran successfully

:return
popd
exit /b %RC%
:errmsg
echo ERROR: %ERRMSG%
SET RC=8
GOTO return               
