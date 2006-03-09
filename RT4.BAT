rem test and verify mz390 > az390 errors (does not stop on error)
call %~dps0mz390 %1 notiming %2 %3 %4 %5 %6 %7 %8 %9 sysmac(%~dps0mac) syscpy(%~dps0mac)
call vdiff %1.bal %1.svb %1.dib 
copy %~dps0test\junk.txt %1.prn
java -classpath %~dps0z390.jar -Xrs az390 %1 notiming %2 %3 %4 %5 %6 %7 %8 %9
call vdiff %1.prn %1.svp %1.dip 
