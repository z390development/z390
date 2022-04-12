REM START CICS TRANSACTION SERVER ON THIS PROCESSOR
SET CICSPATH=cics
SET PROGRAM=Z390CICS
SET Z390INI=%CICSPATH%\%PROGRAM%.INI

REM get the z390 directory (2 levels up: z390\cics\bat)
set "zdir=%~dps0..\..\"
for %%f in ("%zdir%") do set "zdir=%%~ff"
REM remove trailing forward slash
set zdir=%zdir:~0,-1%
REM z390.jar in z390 directory
set JARPATH=%zdir%

set Z390SEQ=%CICSPATH%\seq\

CALL %CICSPATH%\bat\Z390FILE
CALL bat\EZ390 %CICSPATH%\%PROGRAM% NOTIME %1 %2 %3 %4 CON
IF ERRORLEVEL 1 GOTO WRONG
GOTO END
:WRONG
REM ERROR ERROR ERROR
:END
PAUSE VERIFY STARTUP OF CICS
