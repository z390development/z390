rem regression test z390 vsam demos
call rt\RTVSAM1 systerm(vsam\test\rtvsam) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTLIST rt\vsam\dif 
rem end of rtvsam.bat
