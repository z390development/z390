REM ASSEMBLE, LINK, AND EXEC DEMOS
CALL vsam\demo\BUILDCAT %1 %2 %3 %4 %5
CALL vsam\demo\ESF1DEMO %1 %2 %3 %4 %5                
CALL vsam\demo\ESV1DEMO %1 %2 %3 %4 %5                
CALL vsam\demo\RRF1DEMO %1 %2 %3 %4 %5                
CALL vsam\demo\RRV1DEMO %1 %2 %3 %4 %5                
CALL vsam\demo\KSF1RPO1 %1 %2 %3 %4 %5                
CALL vsam\demo\KSF1DEMO %1 %2 %3 %4 %5                
CALL vsam\demo\KSF1SKP1 %1 %2 %3 %4 %5                
