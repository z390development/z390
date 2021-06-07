rem run soa\demo regression tests 
call rt\RTSOA1 NOTIMING %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTLIST rt\soa\dif  
rem end of rtsoa.bat
