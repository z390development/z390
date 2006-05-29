rem regression test z390 demos
erase    %~dps0demo\rtdemo.err
call rt1 %~dps0demo\demo     systerm(rtdemo) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0demo\demowto1 systerm(rtdemo) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0demo\demowto2 systerm(rtdemo) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0demo\demobmk1 systerm(rtdemo) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt2 %~dps0demo\demonum1 systerm(rtdemo) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0demo\demonum2 systerm(rtdemo) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt2 %~dps0demo\demom8q1 systerm(rtdemo) %1 %2 %3 %4 %5 %6 %7 %8 %9
call vdiff %~dps0demo\rtdemo.err %~dps0demo\rtdemo.sve %~dps0demo\rtdemo.die
rem end of rtdemo
