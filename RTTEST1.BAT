rem run rt\test pgms and get rt\dif
rem note testdc1.mlc  > testasc1.mlc for ascii test
rem note testsdt1.mlc > testasc2.mlc for ascii test
rem note testins2.mlc > testasc3.mlc for ascii test
rem note testtst1.mlc > testasc4.mlc for ascii test
rem note testins1.mlc > testasc5.mlc for az390 test
rem note testmac7 and testasm2 must be edited with notepad for tabs
erase %~dps0rt\dif\*.* /q
call RTCLEAN1 rt\test *
rem
rem dynamic linked subroutines used in testlnk1
rem
call RT1 %~dps0rt\test\TESTSUB1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTSUB4 amode24 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem mz390 macro processor
rem
call RT1 %~dps0rt\test\TESTSYS1 sysparm(12345) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTSYN1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTSYN2 sysmac(mac+rt\test) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTPRM1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTPRM2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTPRM3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTMAC1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTMAC2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTMAC3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTMAC4 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTMAC5 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTMAC6 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTMAC7 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTMAC8 sysmac(rt\test+mac) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTMAC9 sysmac(rt\test+mac) syscpy(rt\test) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTPRO1 profile(TESTPRO1) sysmac(rt\test+mac) syscpy(rt\test) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTOPR1 tracep systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTSET1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTSET2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTSET3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTACT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTCPY1 systerm(RTTEST) bal %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTCPY2 systerm(RTTEST) bal %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTSQL1 sysmac(rt\test+mac) syscpy(rt\test) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT3 %~dps0rt\test\TESTEQU1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT3 %~dps0rt\test\TESTERR1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT3 %~dps0rt\test\TESTERR3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTPCH1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTPCH2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
set IN1=rt\test\TESTPCH3.IN1
set IN2=rt\test\TESTPCH3.IN2
set OT1=rt\test\TESTPCH3.OT1
set OT2=rt\test\TESTPCH3.OT2
set OT3=rt\test\TESTPCH3.OT3
call RT5 %~dps0rt\test\TESTPCH3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call MAC %~dps0rt\test\TESTMCR1.ASM sysmac(%~dps0rt\test\*.mcr) notiming systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT3 %~dps0rt\test\TESTTXT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTPC1  tracep systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem az390 assembler
rem
call RT1 %~dps0rt\test\TESTINS1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0rt\test\TESTINS1.BAL %~dps0rt\test\TESTASC5.BAL
call AZ390 %~dps0rt\test\TESTASC5 notiming systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTDST1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTLCT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTSYM1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTUSE1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTUSE2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTPRT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTDC1  systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0rt\test\TESTDC1.MLC %~dps0rt\test\TESTASC1.MLC
call RT1 %~dps0rt\test\TESTASC1 ascii systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTDC2  systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTSDT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTLOC1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0rt\test\TESTSDT1.MLC %~dps0rt\test\TESTASC2.MLC
call RT1 %~dps0rt\test\TESTASC2 ascii systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTASM1 systerm(RTTEST) mcall %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTASM2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT4 %~dps0rt\test\TESTERR2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem lz390 linker
rem
call RT1 %~dps0rt\test\TESTRLD1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTENT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTEXT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTEXT2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTWXT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTOBJ1 objhex systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTOBJ2 objhex mem(49) rmode31 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTPRM4 "parm('a, b')" "sysparm(a, b)" trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem ez390 emulator
rem
call RT7 %~dps0rt\test\TESTERR4 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTERR5 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT7 %~dps0rt\test\TESTERR6 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT7 %~dps0rt\test\TESTERR7 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTINS2 trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0rt\test\TESTINS2.MLC %~dps0rt\test\TESTASC3.MLC
call RT1 %~dps0rt\test\TESTASC3 ascii trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTFP1  trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTDFP1 trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
set DDTODEMO=demo\DEMO.390
call RT1 %~dps0rt\test\TESTCAL1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTCAL2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTLNK1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTLOD1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTLOD2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTXCL3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTXCL2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTXCL1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTDCB1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTDCB2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTDCB3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTDCB4 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTDCB5 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTDCB6 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTDCB7 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTDCB8 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTDCB9 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
erase %~dps0rt\test\TESTDCBA.TF2
erase %~dps0rt\test\TESTDCBA.TF3
call RT1 %~dps0rt\test\TESTDCBA systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTDCBB systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTDCBC ascii systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTDCBD systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTDCBE ascii systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTEOF1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTTIM1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTTIM2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTECB1 trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTMEM1 mem(32)      systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTMEM2 mem(2)       systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTMEM3 mem(32)      systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTMEM4 mem(2)       systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT5 %~dps0rt\test\TESTTST1 test(SYSUT1) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0rt\test\TESTTST1.MLC %~dps0rt\test\TESTASC4.MLC
copy %~dps0rt\test\TESTTST1.TF1 %~dps0rt\test\TESTASC4.TF1
call RT5 %~dps0rt\test\TESTASC4 test(SYSUT1) ascii systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTCMD1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTCMD2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT7 %~dps0rt\test\TESTDMP1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT7 %~dps0rt\test\TESTDMP2 dump systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT7 %~dps0rt\test\TESTDMP3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTDMP4 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT7 %~dps0rt\test\TESTDMP5 dump ascii systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTXLT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTBLD1 sys390(rt\test+demo) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTSVC1 trace systerm(RTTEST) noprotect %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTIPL1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTIPL2 ipl(TESTIPL1) sys390(rt\test) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTZCV1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTSPI1 trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTSTA1 trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTCTD1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTCFD1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTCVB1 trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0rt\test\TESTCVBG trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTCOMP %~dps0rt\test %~dps0rt\test\save %~dps0rt\dif
