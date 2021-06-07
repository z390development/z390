rem regression test z390 assist programs
call rt\RTAST1 systerm(assist\rtast) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTLIST rt\assist\dif 
rem end of rtast.bat
