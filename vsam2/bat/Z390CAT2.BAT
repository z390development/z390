@echo off
REM build Z390CAT2.390 with all VSAM catalog entries for vsam2 test pgms
rem %0 in z390\vsam2\bat; switch to z390
pushd %~dps0..\..

SET VSAMPATH=vsam2
SET ZVSAM=ZVSAM(2)

SET RC=0
CALL bat\ASML %VSAMPATH%\mlc\Z390CAT2 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    ECHO %0 ERROR: unable to build Z390CAT2.390 VSAM catalog
)

:return
popd
exit /b %RC%
