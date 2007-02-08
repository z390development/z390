rem run z390 EXEC CICS regression tests 
call %~dps0RTCICS1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0RTLIST %~dps0cics\dif 
rem end of rtcics.bat
