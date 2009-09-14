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
call rt\RT1 rt\test\TESTSUB1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSUB4 amode24  %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem mz390 macro processor
rem
call rt\RT1 rt\test\TESTSYS1 sysparm(12345)  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSYS2 bs2000  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSYN1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSYN2 sysmac(mac+rt\test)  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTPRM1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTPRM2  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTPRM3  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC2 tracep  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC3  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC4  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC5  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC6  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC7  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC8 sysmac(rt\test+mac)  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMAC9 sysmac(rt\test+mac) syscpy(rt\test)  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTPRO1 @rt\test\TESTPRO1.OPT  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTOPR1 tracep  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTOPT1 @TESTOPT1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTORG1 trace   %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSET1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSET2  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSET3  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTACT1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCPY1  bal %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCPY2  bal %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSQL1 sysmac(rt\test+mac) syscpy(rt\test)  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT3 rt\test\TESTEQU1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT3 rt\test\TESTERR1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT3 rt\test\TESTERR3  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT3 rt\test\TESTERR8  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT3 rt\test\TESTERR9  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTPCH1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTPCH2  %1 %2 %3 %4 %5 %6 %7 %8 %9
set IN1=rt\test\TESTPCH3.IN1
set IN2=rt\test\TESTPCH3.IN2
set OT1=rt\test\TESTPCH3.OT1
set OT2=rt\test\TESTPCH3.OT2
set OT3=rt\test\TESTPCH3.OT3
call rt\RT5 rt\test\TESTPCH3  %1 %2 %3 %4 %5 %6 %7 %8 %9
erase rt\test\TESTMCR1.STA
copy  mac\SUBENTRY.MAC rt\test\SUBENTRY.MCR
copy  mac\SUBEXIT.MAC rt\test\SUBEXIT.MCR
copy  mac\WTO.MAC rt\test\WTO.MCR
call MAC    rt\test\TESTMCR1.ASM stats sysmac(rt\test\*.mcr) notiming  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT3 rt\test\TESTTXT1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTPC1  tracep  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTAIN1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTAIN2 syscpy(+rt\test) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem az390 assembler
rem
call rt\RT1 rt\test\TESTINS1 assist  %1 %2 %3 %4 %5 %6 %7 %8 %9
copy rt\test\TESTINS1.BAL rt\test\TESTASC5.BAL
erase rt\test\TESTASC5.STA
call AZ390 rt\test\TESTASC5 assist notiming  stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTDST1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTLCT1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSYM1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTUSE1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTUSE2  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTPRT1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTDC1   %1 %2 %3 %4 %5 %6 %7 %8 %9
copy rt\test\TESTDC1.MLC rt\test\TESTASC1.MLC
call rt\RT1 rt\test\TESTASC1 ascii  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTDC2   %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSDT1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTLOC1  %1 %2 %3 %4 %5 %6 %7 %8 %9
copy rt\test\TESTSDT1.MLC rt\test\TESTASC2.MLC
call rt\RT1 rt\test\TESTASC2 ascii  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTASM1  mcall SYSMAC(RT\TEST+MAC) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTASM2  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTENV1 TRACE  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT4 rt\test\TESTERR2  ERR(0) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT4 rt\test\TESTERRA  chkmac(2) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT4 rt\test\TESTLAB1  ERR(0) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem lz390 linker
rem
call rt\RT1 rt\test\TESTRLD1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTRLD2  NOLOADHIGH TRACE %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTENT1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTEXT1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTEXT2  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTWXT1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTOBJ1 objhex  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTOBJ2 @TESTOBJ2  %1 %2 %3 %4 %5 %6 %7 %8 %9
rem remove 32 mb file to save space and avoid compare 
erase rt\test\testobj2.390
call rt\RT1 rt\test\TESTPRM4 @TESTPRM4  %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem ez390 emulator
rem
call rt\RT7 rt\test\TESTERR4  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTERR5  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT7 rt\test\TESTERR6  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT7 rt\test\TESTERR7  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTINS2 trace  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTINS3 trace  %1 %2 %3 %4 %5 %6 %7 %8 %9
copy rt\test\TESTINS2.MLC rt\test\TESTASC3.MLC
call rt\RT1 rt\test\TESTASC3 ascii trace  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTFP1  trace  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTFP2  trace  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTDFP1 trace  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTDFP2 trace  %1 %2 %3 %4 %5 %6 %7 %8 %9
set DDTODEMO=demo\DEMO.390
call rt\RT1 rt\test\TESTCAL1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCAL2  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTLNK1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASM rt\test\TESTLNK2  notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASM rt\test\TESTLNK3  notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
SET  MYLIB=rt\test
call LINK rt\test\TESTLNK2  notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call EXEC rt\test\TESTLNK3  notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTLOD1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTLOD2  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTXCL4  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTXCL3  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTXCL2  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTXCL1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB2  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB3  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB4  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB5  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB6  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB7  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB8  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCB9  %1 %2 %3 %4 %5 %6 %7 %8 %9
erase rt\test\TESTDCBA.TF2
erase rt\test\TESTDCBA.TF3
call rt\RT1 rt\test\TESTDCBA  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCBB  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCBC ascii  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCBD  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCBE ascii  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTDCBF  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTEOF1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTTIM1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTTIM2  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTECB1 trace  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMEM1 mem(32)       %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMEM2 mem(2)        %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMEM3 mem(32)       %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTMEM4 mem(2)        %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT5 rt\test\TESTTST1 test(SYSUT1)  %1 %2 %3 %4 %5 %6 %7 %8 %9
copy rt\test\TESTTST1.MLC rt\test\TESTASC4.MLC
copy rt\test\TESTTST1.TF1 rt\test\TESTASC4.TF1
call rt\RT5 rt\test\TESTASC4 test(SYSUT1) ascii  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCMD1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCMD2  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCIC1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASM rt\test\TESTCIC2 cics notiming  sysmac(cics+mac) syscpy(cics+mac) stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT7 rt\test\TESTDMP1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT7 rt\test\TESTDMP2 dump  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT7 rt\test\TESTDMP3  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTDMP4  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT7 rt\test\TESTDMP5 dump ascii  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTXLT1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTBLD1 sys390(rt\test+demo)  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSVC1 trace  noprotect %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTIPL1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTIPL2 ipl(TESTIPL1) sys390(rt\test)  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTZCV1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSPI1 trace  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSTA1 trace  %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASML rt\test\TESTSTA3 notiming  stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSTA2 trace  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCTD1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCFD1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCVB1 trace  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTCVBG trace  %1 %2 %3 %4 %5 %6 %7 %8 %9
rem test zstrmac basic structures using bootstrap version 1
set sysut1=rt\test\testspe1.zsm
set sysut2=rt\test\testspe1.mlc
call MZ390 rt\test\zstrmac1  NOASM STATS NOTIMING %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSPE1  STATS %1 %2 %3 %4 %5 %6 %7 %8 %9
rem translate structured version 2 using bootstrap version 1
set sysut1=rt\test\TESTSPE2.zsm
set sysut2=rt\test\TESTSPE2.mlc
call MZ390 rt\test\zstrmac1  NOASM STATS NOTIMING %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSPE2  STATS %1 %2 %3 %4 %5 %6 %7 %8 %9
rem translate structured version 2 using bootstrap version 1
set sysut1=rt\test\zstrmac2.zsm
set sysut2=rt\test\zstrmac2.mlc
call MZ390 rt\test\zstrmac1  NOASM STATS NOTIMING %1 %2 %3 %4 %5 %6 %7 %8 %9
REM verify TESTZSM1.ZSM translation using zstrmac2 matches zstrmac1
set sysut1=rt\test\testspe1.zsm
set sysut2=rt\test\testspe1.txt
call MZ390 rt\test\zstrmac2  NOASM STATS NOTIMING %1 %2 %3 %4 %5 %6 %7 %8 %9
rem regen ZSTRMAC2 using mz390 support to ver zstrmac2.txt = mlc
set sysut1=rt\test\zstrmac2.zsm
set sysut2=rt\test\zstrmac2.txt
call MZ390 rt\test\zstrmac2.zsm NOASM STATS NOTIMING %1 %2 %3 %4 %5 %6 %7 %8 %9
rem test extensions to ACASE added in zstrmac2 C,X,(v1,v2)
set sysut1=rt\test\testspe3.zsm
set sysut2=rt\test\testspe3.mlc
call MZ390 rt\test\zstrmac2.zsm NOASM STATS NOTIMING %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 rt\test\TESTSPE3  %1 %2 %3 %4 %5 %6 %7 %8 %9
rem test zstrmac error messages
set sysut1=rt\test\testspe4.zsm
set sysut2=rt\test\testspe4.mlc
call MZ390 rt\test\zstrmac2.zsm NOASM STATS NOTIMING %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test ZSTRMAC SPM's
call rt\RT1 rt\test\TESTSPM1  %1 %2 %3 %4 %5 %6 %7 %8 %9
erase rt\test\dif\*.* /q 
call rt\RTCOMP rt\test rt\test\save rt\test\dif
