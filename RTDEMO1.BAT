rem run rtdemo pgms and gen rt\dif
erase %~dps0demo\dif\*.* /q
call RTCLEAN1 demo *
call ASMLG %~dps0demo\DEMO     notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASMLG %~dps0demo\DEMOWTO1 notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASMLG %~dps0demo\DEMOWTO2 notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASMLG %~dps0demo\DEMOBMK1 notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call MAC %~dps0demo\DEMONUM1   notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASMLG %~dps0demo\DEMONUM2 notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASMLG %~dps0demo\DEMODFP1 notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call MAC %~dps0demo\DEMOM8Q1   nolistcall notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASMLG %~dps0demo\STDDEVLB notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTCOMP %~dps0demo %~dps0demo\save %~dps0demo\dif
