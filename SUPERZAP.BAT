run SUPERXAP using console or SYSIN=%1.ZAP and SYSPRINT=%1.RPT
if "%1" == "" goto :console
:batch
set SYSIN=%1.ZAP
set SYSPRINT=%1.RPT
call EXEC SUPERZAP %2 %3 %4 %5
goto :end
:console
call EXEC SUPERZAP PARM(CON) %1 %2 %3 %4 %5
:end
if errorlevel 1 pause error occurred in superzap
