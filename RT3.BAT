rem test and verify mz390 errors (doest not stop on error)
erase %1.err
copy %~dps0test\junk.txt %1.bal
java -classpath %~dps0z390.jar -Xrs mz390 %1 notiming %2 %3 %4 %5 %6 %7 %8 %9 sysmac(%~dps0mac) syscpy(%~dps0mac)
call vdiff %1.bal %1.svb %1.dib 
