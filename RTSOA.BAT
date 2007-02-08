rem run soa\demo regression tests 
call %~dps0RTSOA1 NOTIMING %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTLIST %~dps0soa\dif  
rem end of rtsoa.bat
