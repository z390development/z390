@echo off
setlocal
rem Assemble, link, and exec ASSIST program %1
set XREAD=%1.XRD
set XPRNT=%1.XPR
set XPNCH=%1.XPH
set XGET=%1.XGT
set XPUT=%1.XPT
call %~dps0ASMLG %1 assist %2 %3 %4 %5 %6 %7 %8 %9
if %ERRORLEVEL% NEQ 0 (
    echo ASSIST   ERROR: Encountered RC %ERRORLEVEL% - exiting
    exit /b %ERRORLEVEL%
)
