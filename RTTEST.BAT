rem run z390 regression tests 
rem note testdc1.mlc  > testasc1.mlc for ascii test
rem note testsdt1.mlc > testasc2.mlc for ascii test
rem note testins2.mlc > testasc3.mlc for ascii test
rem note testtst1.mlc > testasc4.mlc for ascii test
rem note testmac7 and testasm2 must be edited with notepad for tabs
erase %~dps0test\rttest.err
rem
rem dynamic linked subroutines used in testlnk1
rem
call rt1 %~dps0test\testsub1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testsub4 amode24 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem mz390 macro processor
rem
call rt1 %~dps0test\testsys1 sysparm(12345) systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testsyn1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testsyn2 sysmac(mac+test) systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testprm1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testprm2 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testprm3 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmac1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmac2 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmac3 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmac4 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmac5 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmac6 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmac7 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmac8 sysmac(test+mac) systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmac9 sysmac(test+mac) syscpy(test) systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testpro1 profile(testpro1) sysmac(test+mac) syscpy(test) systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testopr1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testset1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testset2 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testset3 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testact1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testcpy1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testsql1 sysmac(test+mac) syscpy(test) systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt3 %~dps0test\testequ1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt3 %~dps0test\testerr1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt3 %~dps0test\testerr3 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testpch1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call vdiff %~dps0test\testpch1.pch %~dps0test\testpch1.svh test\testpch1.dih
call rt5 %~dps0test\testpch2 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call mz390 %~dps0test\testmcr1.asm sysmac(%~dps0test\*.mcr) notiming systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call vdiff %~dps0test\testmcr1.bal %~dps0test\testmcr1.svb test\testmcr1.dib
call rt3 %~dps0test\testtxt1 text systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call vdiff %~dps0test\testtxt1a.html %~dps0test\testtxt1a.sv1 test\testtxt1.di1
call vdiff %~dps0test\testtxt1b.html %~dps0test\testtxt1b.sv2 test\testtxt1.di2
rem
rem az390 assembler
rem
call rt1 %~dps0test\testins1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testdst1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testlct1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testsym1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testuse1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testuse2 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testprt1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testdc1  systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0test\testdc1.mlc %~dps0test\testasc1.mlc
call rt1 %~dps0test\testasc1 ascii systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testsdt1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testloc1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0test\testsdt1.mlc %~dps0test\testasc2.mlc
call rt1 %~dps0test\testasc2 ascii systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testasm1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testasm2 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt4 %~dps0test\testerr2 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem lz390 linker
rem
call rt1 %~dps0test\testrld1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testent1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testext1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testext2 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testwxt1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testobj1 objhex systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testobj2 objhex mem(49) rmode31 trace systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testprm4 "parm('a, b')" "sysparm(a, b)" trace systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem ez390 emulator
rem
call rt7 %~dps0test\testerr4 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testerr5 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt7 %~dps0test\testerr6 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt7 %~dps0test\testerr7 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testins2 trace systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0test\testins2.mlc %~dps0test\testasc3.mlc
call rt1 %~dps0test\testasc3 ascii trace systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testfp1  trace systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
set DDTODEMO=DEMO\DEMO.390
call rt1 %~dps0test\testcal1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testcal2 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testlnk1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testlod1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testlod2 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testxcl3 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testxcl2 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testxcl1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb2 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb3 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb4 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb5 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb6 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb7 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb8 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcb9 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
erase %~dps0test\testdcba.tf2
erase %~dps0test\testdcba.tf3
call rt1 %~dps0test\testdcba systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call vdiff %~dps0test\testdcba.tf1 %~dps0test\testdcba.sf1 %~dps0test\testdcba.df1
call vdiff %~dps0test\testdcba.tf2 %~dps0test\testdcba.sf2 %~dps0test\testdcba.df2
call vdiff %~dps0test\testdcba.tf3 %~dps0test\testdcba.sf3 %~dps0test\testdcba.df3
call rt5 %~dps0test\testdcbb systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcbc ascii systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcbd systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testdcbe ascii systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testeof1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testtim1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testtim2 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testecb1 trace systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmem1 mem(32)      systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmem2 mem(2)       systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testmem3 mem(32)      systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0test\testtst1 test(sysut1) systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0test\testtst1.mlc %~dps0test\testasc4.mlc
copy %~dps0test\testtst1.tf1 %~dps0test\testasc4.tf1
call rt5 %~dps0test\testasc4 test(sysut1) ascii systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testcmd1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testcmd2 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt7 %~dps0test\testdmp1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt7 %~dps0test\testdmp2 dump systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt7 %~dps0test\testdmp3 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testdmp4 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt7 %~dps0test\testdmp5 dump ascii systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testxlt1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testbld1 sys390(test+demo) systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testsvc1 trace systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testipl1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testipl2 ipl(testipl1) sys390(test) systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testzcv1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testspi1 trace systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\teststa1 trace systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testctd1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testcfd1 systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testcvb1 trace systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0test\testcvbg trace systerm(rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call vdiff %~dps0test\rttest.err %~dps0test\rttest.sve %~dps0test\rttest.die
rem end of rttest
