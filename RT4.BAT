rem test and verify mz390 > az390 errors (does not stop on error)
call mz390 %1 notiming %2 %3 %4 %5 %6 %7 %8 %9 sysmac(mac) syscpy(mac)
call vdiff %1.bal %1.svb %1.dib 
copy test\junk.txt %1.prn
java -classpath z390.jar -Xrs az390 %1 notiming %2 %3 %4 %5 %6 %7 %8 %9
call vdiff %1.prn %1.svp %1.dip 
