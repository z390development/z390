rem test and verify mz390 > az390 errors (does not stop on error)
erase %1.err
call %~dps0mz390 %1 bal notiming  sysmac(%~dps0mac) syscpy(%~dps0mac) %2 %3 %4 %5 %6 %7 %8 %9
