rem test and verify mz390 errors (doest not stop on error)
copy test\junk.txt %1.bal
java -classpath z390.jar -Xrs mz390 %1 notiming %2 %3 %4 %5 %6 %7 %8 %9 sysmac(mac) syscpy(mac)
:end
call vdiff %1.bal %1.svb %1.dib 
