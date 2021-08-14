@echo off
setlocal
pushd %~dp0
cd..\..
rem regression test z390 instructions
set SNAPOUT=ZOPCHECK\SNAPOUT.TXT
call bat\asmlg zopcheck\ZOPCHECK bal trace sysmac(mac) syscpy(zopcheck+mac) optable(z390)
if %ERRORLEVEL% NEQ 0 (
    echo ZOPCHECK ERROR: ASMLG returned non-zero result
)
exit /b %ERRORLEVEL%
