rem run rt\test regression tests 
call rt\RTTEST1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTLIST rt\test\dif  
rem end of rttest.bat
