rem test and verify asml only
erase %1.err
erase %1.bal
erase %1.obj
erase %1.prn
erase %1.lst
erase %1.390
call %~dps0asml %1 bal notiming %2 %3 %4 %5 %6 %7 %8 %9
