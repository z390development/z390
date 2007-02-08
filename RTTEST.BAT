rem run rt\test regression tests 
call %~dps0RTTEST1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTLIST %~dps0rt\dif  
rem end of rttest.bat
