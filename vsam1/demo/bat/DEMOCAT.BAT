@echo off
REM build DEMOCAT.390 with all VSAM catalog entries for vsam\demo pgms

rem %0 in z390\vsam1\demo\bat; switch to z390
pushd %~dps0..\..\..

SET VSAMPATH=vsam1\demo
SET ZVSAM=ZVSAM(1)

SET RC=0
CALL bat\ASML %VSAMPATH%\DEMOCAT %1 %2 %3 %4 %5 %ZVSAM%
SET RC=%ERRORLEVEL%
IF %RC% NEQ 0 (
    ECHO ERROR: unable to build DEMOCAT.390 VSAM catalog
)
:return
popd
exit /b %RC%
