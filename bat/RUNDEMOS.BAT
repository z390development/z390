rem demo hello world demo
cd..
call bat\ASMLG demo\DEMO sysmac(mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
pause verify demo rc=0 and hello world wto 
call bat\ASM demo\DEMOM8Q1 %1 %2 %3 %4 %5 %6 %7 %8 %9
pause verify demo rc=0 see prn file for 8 queens solutions
set sysut1=demo\TESTDCB1.TF1
set sysut2=demo\TESTDCB1.TF2
set sysout=demo\TESTDCB1.TF3
call bat\ASMLG demo\TESTDCB1 sysmac(mac) %1 %2 %3 %4 %5 %6 %7 %8 %9  
type demo\TESTDCB1.TF2
pause verify demo\TESTDCB1 copied TESTDCB1.TF1 to TESTDCB1.TF2