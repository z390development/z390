rem run rtdemo pgms and gen rt\dif
erase %~dps0demo\dif\*.* /q
call rtclean1 demo *
call asmlg %~dps0demo\demo     notiming systerm(rtdemo) %1 %2 %3 %4 %5 %6 %7 %8 %9
call asmlg %~dps0demo\demowto1 notiming systerm(rtdemo) %1 %2 %3 %4 %5 %6 %7 %8 %9
call asmlg %~dps0demo\demowto2 notiming systerm(rtdemo) %1 %2 %3 %4 %5 %6 %7 %8 %9
call asmlg %~dps0demo\demobmk1 notiming systerm(rtdemo) %1 %2 %3 %4 %5 %6 %7 %8 %9
call mac %~dps0demo\demonum1   notiming systerm(rtdemo) %1 %2 %3 %4 %5 %6 %7 %8 %9
call asmlg %~dps0demo\demonum2 notiming systerm(rtdemo) %1 %2 %3 %4 %5 %6 %7 %8 %9
call mac %~dps0demo\demom8q1   nolistcall notiming systerm(rtdemo) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rtcomp %~dps0demo %~dps0demo\save %~dps0demo\dif
