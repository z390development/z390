rem run z390 regression tests
rem
rem dynamic linked subroutines used in testlnk1
rem
call rt1 test\testsub1
call rt1 test\testsub4 amode24
rem
rem mz390 macro processor
rem
call rt1 test\testprm1
call rt1 test\testprm2
call rt1 test\testprm3
call rt1 test\testmac1
call rt1 test\testmac2
call rt1 test\testset1
call rt1 test\testset2
call rt1 test\testset3
call rt1 test\testact1
call rt1 test\testcpy1
call rt3 test\testerr1
call rt5 test\testpch1
call vdiff test\testpch1.pch test\testpch1.svh test\testpch1.dih
call rt5 test\testpch2
rem
rem az390 assembler
rem
call rt1 test\testins1
call rt1 test\testdst1
call rt1 test\testlct1
call rt1 test\testsym1
call rt1 test\testuse1
call rt1 test\testdc1
call rt1 test\testsdt1
call rt1 test\testasm1
call rt4 test\testerr2
rem
rem lz390 linker
rem
call rt1 test\testrld1
call rt1 test\testext1
call rt1 test\testext2
rem
rem ez390 emulator
rem
call rt1 test\testins2
call rt1 test\testfp1
call rt1 test\testlnk1
call rt5 test\testdcb1
call rt5 test\testdcb2
call rt5 test\testdcb3
call rt5 test\testdcb4
call rt5 test\testdcb5
call rt5 test\testdcb6
call rt5 test\testdcb7
call rt5 test\testdcb8
call rt5 test\testdcb9
call rt1 test\testtim1
call rt1 test\testmem1 mem(32)
call rt5 test\testtst1 test(sysut1)
rem end of rttest
