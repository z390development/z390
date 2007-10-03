PAUSE run vsam REPRO utility to load or unload with INFILE=%1 and OUTFILE=%2
set INFILE=%1
set OUTFILE=%2
call EXEC REPRO
if errorlevel 1 pause error occurred running REPRO
