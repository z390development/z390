rem run z390 regression tests 
rem note testdc1.mlc  > testasc1.mlc for ascii test
rem note testsdt1.mlc > testasc2.mlc for ascii test
rem note testins2.mlc > testasc3.mlc for ascii test
rem note testtst1.mlc > testasc4.mlc for ascii test
rem note testmac7 and testasm2 must be edited with notepad for tabs
rem
rem dynamic linked subroutines used in testlnk1
rem
call rt1 %~dps0test\testsub1
call rt1 %~dps0test\testsub4 amode24
rem
rem mz390 macro processor
rem
call rt1 %~dps0test\testsys1 sysparm(12345) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testsyn1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testprm1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testprm2 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testprm3 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmac1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmac2 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmac3 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmac4 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmac5 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmac6 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testopr1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testset1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testset2 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testset3 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testact1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testcpy1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt3 %~dps0test\testequ1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt3 %~dps0test\testerr1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt3 %~dps0test\testerr3 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testpch1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call vdiff %~dps0test\testpch1.pch %~dps0test\testpch1.svh test\testpch1.dih
call rt5 %~dps0test\testpch2 %1 %2 %3 %4 %5 %6 %7 %8 %9
call mz390 %~dps0test\testmcr1.asm sysmac(%~dps0test\*.mcr) notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call vdiff %~dps0test\testmcr1.bal %~dps0test\testmcr1.svb test\testmcr1.dib
rem
rem az390 assembler
rem
call rt1 %~dps0test\testins1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testdst1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testlct1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testsym1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testuse1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testuse2 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testprt1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testdc1  %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0test\testdc1.mlc %~dps0test\testasc1.mlc
call rt1 %~dps0test\testasc1 ascii %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testsdt1 %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0test\testsdt1.mlc %~dps0test\testasc2.mlc
call rt1 %~dps0test\testasc2 ascii %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testasm1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testasm2 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt4 %~dps0test\testerr2 %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem lz390 linker
rem
call rt1 %~dps0test\testrld1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testext1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testext2 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testwxt1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testobj1 objhex %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testobj2 objhex mem(49) rmode31 trace %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testprm4 "parm('a, b')" "sysparm(a, b)" trace %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem ez390 emulator
rem
call rt1 %~dps0test\testerr4 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testerr5 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testerr6 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testins2 trace %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0test\testins2.mlc %~dps0test\testasc3.mlc
call rt1 %~dps0test\testasc3 ascii trace %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testfp1  trace %1 %2 %3 %4 %5 %6 %7 %8 %9
set DDTODEMO=DEMO\DEMO.390
call rt1 %~dps0test\testcal1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testcal2 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testlnk1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testlod1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testlod2 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testxcl3 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testxcl2 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testxcl1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb2 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb3 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb4 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb5 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb6 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb7 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb8 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb9 %1 %2 %3 %4 %5 %6 %7 %8 %9
erase %~dps0test\testdcba.tf2
erase %~dps0test\testdcba.tf3
call rt1 %~dps0test\testdcba %1 %2 %3 %4 %5 %6 %7 %8 %9
call vdiff %~dps0test\testdcba.tf1 %~dps0test\testdcba.sf1 %~dps0test\testdcba.df1
call vdiff %~dps0test\testdcba.tf2 %~dps0test\testdcba.sf2 %~dps0test\testdcba.df2
call vdiff %~dps0test\testdcba.tf3 %~dps0test\testdcba.sf3 %~dps0test\testdcba.df3
call rt5 %~dps0test\testdcbb %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcbc ascii %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcbd %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcbe ascii %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testeof1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testtim1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testtim2 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmem1 mem(32)      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmem2 mem(2)       %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmem3 mem(32)      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testtst1 test(sysut1) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0test\testtst1.mlc %~dps0test\testasc4.mlc
call rt5 %~dps0test\testasc4 test(sysut1) ascii %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testcmd1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testcmd2 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testdmp1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testdmp2 dump %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testdmp3 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testdmp4 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testdmp5 dump ascii %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testxlt1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testsvc1 trace %1 %2 %3 %4 %5 %6 %7 %8 %9
rem end of rttest
