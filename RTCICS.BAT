rem run z390 EXEC CICS regression tests 
call %~dps0rtcics1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0rtlist %~dps0cics\dif 
rem end of rtcics.bat
