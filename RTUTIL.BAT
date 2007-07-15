rem build and run UTIL regression tests 
call RTCLEAN1 util\unref *
call RTCLEAN1 util\superzap *
call %~dps0RTUTIL1 NOTIMING %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTLIST %~dps0UTIL\dif  
rem end of RTUTIL.BAT
