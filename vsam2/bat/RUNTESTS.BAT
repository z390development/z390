@echo off
REM ASSEMBLE, LINK TEST PROGRAMS AND FILES for ZVSAM(2)

rem %0 in z390\vsam2\bat; switch to z390
pushd %~dps0..\..

SET VSAMPATH=vsam2
SET ZVSAM=ZVSAM(2)

SET RC=0

REM assemble and link a VSAM catalog
CALL %VSAMPATH%\bat\Z390CAT2 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=Z390CAT2 assembly/link error
    GOTO errmsg
)
REM assemble and link zVSAM OPEN module
CALL %VSAMPATH%\bat\ZVSAM19C %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=ZVSAM19C assembly/link error
    GOTO errmsg
)
REM GENCB test
CALL %VSAMPATH%\bat\TESTGNCB %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=TESTGNCB assembly/link error
    GOTO errmsg
)
REM MODCB test
CALL %VSAMPATH%\bat\TESTMDCB %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=TESTMDCB assembly/link error
    GOTO errmsg
)
REM SHOWCB test
CALL %VSAMPATH%\bat\TESTSHCB %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=TESTSHCB assembly/link error
    GOTO errmsg
)
REM TESTCB test
CALL %VSAMPATH%\bat\TESTTSCB %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    SET ERRMSG=TESTTSCB assembly/link error
    GOTO errmsg
)

:return
popd
exit /b %RC%
:errmsg
echo ERROR: %ERRMSG%
SET RC=8
GOTO return