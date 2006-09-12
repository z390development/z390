rem test and verify mz390 errors (doest not stop on error)
erase %1.err
java -classpath %~dps0z390.jar -Xrs mz390 %1 noasm bal notiming  sysmac(%~dps0mac) syscpy(%~dps0mac) %2 %3 %4 %5 %6 %7 %8 %9
