REM read %1.PRN and generated unreferenced symbol report %1.URF
SET PRNFILE=%1.PRN
SET URFFILE=%1.URF
REM CALL EZ390 UNREF ASCII %2 %3 %4 %5
CALL ASMLG linklib\UNREF ASCII SYSCPY(mac+linklib) %1 %2 %3 %4 %5
