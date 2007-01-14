rem run rt\test pgms and get rt\dif
rem note testdc1.mlc  > testasc1.mlc for ascii test
rem note testsdt1.mlc > testasc2.mlc for ascii test
rem note testins2.mlc > testasc3.mlc for ascii test
rem note testtst1.mlc > testasc4.mlc for ascii test
rem note testins1.mlc > testasc5.mlc for az390 test
rem note testmac7 and testasm2 must be edited with notepad for tabs
erase %~dps0rt\dif\*.* /q
call rtclean1 rt\test *
rem
rem dynamic linked subroutines used in testlnk1
rem
call rt1 %~dps0rt\test\testsub1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testsub4 amode24 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem mz390 macro processor
rem
call rt1 %~dps0rt\test\testsys1 sysparm(12345) systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testsyn1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testsyn2 sysmac(mac+rt\test) systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testprm1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testprm2 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testprm3 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testmac1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testmac2 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testmac3 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testmac4 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testmac5 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testmac6 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testmac7 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testmac8 sysmac(rt\test+mac) systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testmac9 sysmac(rt\test+mac) syscpy(rt\test) systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testpro1 profile(testpro1) sysmac(rt\test+mac) syscpy(rt\test) systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testopr1 tracep systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testset1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testset2 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testset3 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testact1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testcpy1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testsql1 sysmac(rt\test+mac) syscpy(rt\test) systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt3 %~dps0rt\test\testequ1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt3 %~dps0rt\test\testerr1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt3 %~dps0rt\test\testerr3 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testpch1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testpch2 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
set IN1=rt\test\testpch3.in1
set IN2=rt\test\testpch3.in2
set OT1=rt\test\testpch3.ot1
set OT2=rt\test\testpch3.ot2
set OT3=rt\test\testpch3.ot3
call rt5 %~dps0rt\test\testpch3 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call mac %~dps0rt\test\testmcr1.asm sysmac(%~dps0rt\test\*.mcr) notiming systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt3 %~dps0rt\test\testtxt1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testpc1  tracep systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem az390 assembler
rem
call rt1 %~dps0rt\test\testins1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0rt\test\testins1.bal %~dps0rt\test\testasc5.bal
call az390 %~dps0rt\test\testasc5 notiming systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testdst1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testlct1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testsym1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testuse1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testuse2 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testprt1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testdc1  systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0rt\test\testdc1.mlc %~dps0rt\test\testasc1.mlc
call rt1 %~dps0rt\test\testasc1 ascii systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testdc2  systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testsdt1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testloc1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0rt\test\testsdt1.mlc %~dps0rt\test\testasc2.mlc
call rt1 %~dps0rt\test\testasc2 ascii systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testasm1 systerm(rt\test\rttest) mcall %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testasm2 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt4 %~dps0rt\test\testerr2 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem lz390 linker
rem
call rt1 %~dps0rt\test\testrld1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testent1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testext1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testext2 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testwxt1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testobj1 objhex systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testobj2 objhex mem(49) rmode31 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testprm4 "parm('a, b')" "sysparm(a, b)" trace systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem ez390 emulator
rem
call rt7 %~dps0rt\test\testerr4 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testerr5 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt7 %~dps0rt\test\testerr6 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt7 %~dps0rt\test\testerr7 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testins2 trace systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0rt\test\testins2.mlc %~dps0rt\test\testasc3.mlc
call rt1 %~dps0rt\test\testasc3 ascii trace systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testfp1  trace systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testdfp1 trace systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
set DDTODEMO=DEMO\DEMO.390
call rt1 %~dps0rt\test\testcal1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testcal2 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testlnk1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testlod1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testlod2 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testxcl3 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testxcl2 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testxcl1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testdcb1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testdcb2 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testdcb3 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testdcb4 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testdcb5 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testdcb6 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testdcb7 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testdcb8 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testdcb9 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
erase %~dps0rt\test\testdcba.tf2
erase %~dps0rt\test\testdcba.tf3
call rt1 %~dps0rt\test\testdcba systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testdcbb systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testdcbc ascii systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testdcbd systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testdcbe ascii systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testeof1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testtim1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testtim2 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testecb1 trace systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testmem1 mem(32)      systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testmem2 mem(2)       systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testmem3 mem(32)      systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testmem4 mem(2)       systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt5 %~dps0rt\test\testtst1 test(sysut1) systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0rt\test\testtst1.mlc %~dps0rt\test\testasc4.mlc
copy %~dps0rt\test\testtst1.tf1 %~dps0rt\test\testasc4.tf1
call rt5 %~dps0rt\test\testasc4 test(sysut1) ascii systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testcmd1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testcmd2 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt7 %~dps0rt\test\testdmp1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt7 %~dps0rt\test\testdmp2 dump systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt7 %~dps0rt\test\testdmp3 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testdmp4 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt7 %~dps0rt\test\testdmp5 dump ascii systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testxlt1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testbld1 sys390(rt\test+demo) systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testsvc1 trace systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testipl1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testipl2 ipl(testipl1) sys390(rt\test) systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testzcv1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testspi1 trace systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\teststa1 trace systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testctd1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testcfd1 systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testcvb1 trace systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0rt\test\testcvbg trace systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rtcomp %~dps0rt\test %~dps0rt\test\save %~dps0rt\dif
