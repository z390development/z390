rem regression test z390 vsam demos
call %~dps0RTVSAM1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0RTLIST %~dps0vsam\test\dif 
call %~dps0RTLIST %~dps0vsam\demo\dif 
rem end of rtvsam.bat
