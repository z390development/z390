rem run rtast pgms and gen rt\assist\dif
call rt\RTCLEAN1 assist *
call ASSIST assist\DEMOAST1 notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASSIST assist\TESTAST1 notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASSIST assist\solp06   notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
erase rt\assist\dif\*.* /q
call rt\RTCOMP assist rt\assist\save rt\assist\dif
