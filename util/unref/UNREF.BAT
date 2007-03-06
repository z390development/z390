REM read %.PRN and generated unreferenced symbol report %2,URF
SET PRNFILE=%1.PRN
SET URFFILE=%1.URF
CALL EZ390 util\unref\UNREF ASCII
