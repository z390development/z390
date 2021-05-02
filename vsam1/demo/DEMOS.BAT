REM ASSEMBLE, LINK, AND EXEC DEMOS
cd..
cd..
CALL vsam1\demo\DEMOCAT %1 %2 %3 %4 %5 zvsam(1)
CALL vsam1\demo\ESF1DEMO %1 %2 %3 %4 %5 zvsam(1)                
CALL vsam1\demo\ESV1DEMO %1 %2 %3 %4 %5 zvsam(1)                
CALL vsam1\demo\RRF1DEMO %1 %2 %3 %4 %5 zvsam(1)                
CALL vsam1\demo\RRV1DEMO %1 %2 %3 %4 %5 zvsam(1)                
CALL vsam1\demo\KSF1RPO1 %1 %2 %3 %4 %5 zvsam(1)                
CALL vsam1\demo\KSF1DEMO %1 %2 %3 %4 %5 zvsam(1)                
CALL vsam1\demo\KSF1SKP1 %1 %2 %3 %4 %5 zvsam(1)                
