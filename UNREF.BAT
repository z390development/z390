REM read %1.PRN and generated unreferenced symbol report %1.URF
SET PRNFILE=%1.PRN
SET URFFILE=%1.URF
CALL EXEC UNREF ASCII %2 %3 %4 %5
if errorlevel 1 pause error occurred executing UNREF
