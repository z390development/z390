rem run rtdemo pgms and gen rt\dif
call rt\RTCLEAN1 demo *
call ASMLG demo\DEMO     notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASMLG demo\DEMOWTO1 notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASMLG demo\DEMOWTO2 notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASMLG demo\DEMOBMK1 notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASMLG demo\SIEVE    notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call MAC demo\DEMONUM1   notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASMLG demo\DEMONUM2 notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASMLG demo\DEMODFP1 notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call MAC demo\DEMOM8Q1   nolistcall notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASMLG demo\STDDEVLB notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
erase rt\demo\dif\*.* /q
call rt\RTCOMP demo rt\demo\save rt\demo\dif
