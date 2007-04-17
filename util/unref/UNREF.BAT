REM read %1.PRN and generated unreferenced symbol report %1.URF
SET PRNFILE=%1.PRN
SET URFFILE=%1.URF
CALL EZ390 util\unref\UNREF ASCII %2 %3 %4 %5
