rem regression test z390 sort utility
call rt\RTSORT1 systerm(sort\rtsort) stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTLIST rt\sort\dif 
rem end of rtsort.bat
