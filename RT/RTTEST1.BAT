rem run rt\test pgms and get rt\dif
rem note testdc1.mlc  > testasc1.mlc for ascii test
rem note testsdt1.mlc > testasc2.mlc for ascii test
rem note testins2.mlc > testasc3.mlc for ascii test
rem note testtst1.mlc > testasc4.mlc for ascii test
rem note testins1.mlc > testasc5.mlc for az390 test
rem note testmac7 and testasm2 must be edited with notepad for tabs
call rt\RTCLEAN1 rt\test *
rem
rem dynamic linked subroutines used in testlnk1
rem
call rt\RT1 rt\test\TESTSUB1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSUB4 amode24 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem mz390 macro processor
rem
call rt\RT1 rt\test\TESTSYS1 sysparm(12345) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSYS2 bs2000 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSYN1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSYN2 sysmac(mac+rt\test) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTPRM1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTPRM2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTPRM3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC2 tracep systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC4 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC5 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC6 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC7 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC8 sysmac(rt\test+mac) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC9 sysmac(rt\test+mac) syscpy(rt\test) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTPRO1 @rt\test\TESTPRO1.OPT systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTOPR1 tracep systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTOPT1 @TESTOPT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTORG1 trace  systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSET1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSET2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSET3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTACT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCPY1 systerm(RTTEST) bal %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCPY2 systerm(RTTEST) bal %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSQL1 sysmac(rt\test+mac) syscpy(rt\test) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT3 rt\test\TESTEQU1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT3 rt\test\TESTERR1 systerm(RTTEST) chkmac(2) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT3 rt\test\TESTERR3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTPCH1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTPCH2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
set IN1=rt\test\TESTPCH3.IN1
set IN2=rt\test\TESTPCH3.IN2
set OT1=rt\test\TESTPCH3.OT1
set OT2=rt\test\TESTPCH3.OT2
set OT3=rt\test\TESTPCH3.OT3
call rt\RT5 rt\test\TESTPCH3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
erase rt\test\TESTMCR1.STA
call MAC    rt\test\TESTMCR1.ASM stats sysmac(rt\test\*.mcr) notiming systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT3 rt\test\TESTTXT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTPC1  tracep systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem az390 assembler
rem
call rt\RT1 rt\test\TESTINS1 assist systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy rt\test\TESTINS1.BAL rt\test\TESTASC5.BAL
erase rt\test\TESTASC5.STA
call AZ390 rt\test\TESTASC5 assist notiming systerm(RTTEST) stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTDST1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTLCT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSYM1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTUSE1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTUSE2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTPRT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTDC1  systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy rt\test\TESTDC1.MLC rt\test\TESTASC1.MLC
call rt\RT1 rt\test\TESTASC1 ascii systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTDC2  systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSDT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTLOC1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy rt\test\TESTSDT1.MLC rt\test\TESTASC2.MLC
call rt\RT1 rt\test\TESTASC2 ascii systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTASM1 systerm(RTTEST) mcall SYSMAC(RT\TEST+MAC) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTASM2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTENV1 TRACE systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT4 rt\test\TESTERR2 systerm(RTTEST) ERR(0) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT4 rt\test\TESTLAB1 systerm(RTTEST) ERR(0) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem lz390 linker
rem
call rt\RT1 rt\test\TESTRLD1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTENT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTEXT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTEXT2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTWXT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTOBJ1 objhex systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTOBJ2 @TESTOBJ2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTPRM4 @TESTPRM4 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem ez390 emulator
rem
call rt\RT7 rt\test\TESTERR4 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTERR5 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT7 rt\test\TESTERR6 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT7 rt\test\TESTERR7 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTINS2 trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTINS3 trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy rt\test\TESTINS2.MLC rt\test\TESTASC3.MLC
call rt\RT1 rt\test\TESTASC3 ascii trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTFP1  trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTDFP1 trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTDFP2 trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
set DDTODEMO=demo\DEMO.390
call rt\RT1 rt\test\TESTCAL1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCAL2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTLNK1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASM rt\test\TESTLNK2 systerm(RTTEST) notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASM rt\test\TESTLNK3 systerm(RTTEST) notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
SET  MYLIB=rt\test
call LINK rt\test\TESTLNK2 systerm(RTTEST) notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call EXEC rt\test\TESTLNK3 systerm(RTTEST) notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTLOD1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTLOD2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTXCL4 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTXCL3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTXCL2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTXCL1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB4 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB5 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB6 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB7 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB8 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB9 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
erase rt\test\TESTDCBA.TF2
erase rt\test\TESTDCBA.TF3
call rt\RT1 rt\test\TESTDCBA systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCBB systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCBC ascii systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCBD systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCBE ascii systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTEOF1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTTIM1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTTIM2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTECB1 trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMEM1 mem(32)      systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMEM2 mem(2)       systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMEM3 mem(32)      systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMEM4 mem(2)       systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTTST1 test(SYSUT1) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy rt\test\TESTTST1.MLC rt\test\TESTASC4.MLC
copy rt\test\TESTTST1.TF1 rt\test\TESTASC4.TF1
call rt\RT5 rt\test\TESTASC4 test(SYSUT1) ascii systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCMD1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCMD2 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCIC1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASM rt\test\TESTCIC2 cics notiming systerm(RTTEST) sysmac(cics+mac) syscpy(cics+mac) stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT7 rt\test\TESTDMP1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT7 rt\test\TESTDMP2 dump systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT7 rt\test\TESTDMP3 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTDMP4 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT7 rt\test\TESTDMP5 dump ascii systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTXLT1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTBLD1 sys390(rt\test+demo) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSVC1 trace systerm(RTTEST) noprotect %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTIPL1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTIPL2 ipl(TESTIPL1) sys390(rt\test) systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTZCV1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSPI1 trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSTA1 trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASML rt\test\TESTSTA3 notiming systerm(RTTEST) stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSTA2 trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCTD1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCFD1 systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCVB1 trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCVBG trace systerm(RTTEST) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTCOMP rt\test rt\test\save rt\test\dif
