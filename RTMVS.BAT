rem run z390 mvs 3.8 regression tests 
rem
rem assemble, link, and execute demo wto 'hello world'
rem
call rt1 %~dps0mvs\demo\demo sysmac(mvs\maclib+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem assemble common application macros as test (not executable pgm)
rem
call rt6 %~dps0mvs\test\testmvs1 sysmac(mvs\maclib+mac) objhex %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt6 %~dps0mvs\test\testcvtx sysmac(mvs\maclib+mac) objhex %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt6 %~dps0mvs\test\testcvt1 sysmac(mvs\maclib+mac) objhex %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem assemble,link, and execute text edit utility
rem   sysut1 is set to input text file 
rem   sysut2 is set to output text file
rem
call rt5 %~dps0mvs\test\testfix1 sysmac(mvs\maclib+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
