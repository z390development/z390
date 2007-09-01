rem regression test z390 vsam demos
call rtclean1 vsam\test 
call %~dps0RTVSAM1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0RTLIST %~dps0vsam\test\dif 
rem end of rtvsam.bat
