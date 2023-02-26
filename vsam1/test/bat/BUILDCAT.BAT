@echo off
REM build TESTCAT.390 with all VSAM catalog entries for vsam\test pgms

rem %0 in z390\vsam1\demo\bat; switch to z390
pushd %~dps0..\..\..

SET VSAMPATH=vsam1\test
SET ZVSAM=ZVSAM(1)

SET RC=0
CALL bat\ASML %VSAMPATH%\TESTCAT %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    ECHO %0 ERROR: unable to build TESTCAT.390 VSAM catalog
)

:return
popd
exit /b %RC%
