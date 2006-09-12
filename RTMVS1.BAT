rem run mvs\test and mvs\demo pgms and get mvs\dif
rem after installing z390, you can download MVS\MVSLIB from www.z390.org
rem rt1 - assemble, link, and execute with notiming
rem rt6 - assemble and link with notiming
rem rtcomp - compare mvs\test files with mvs\save files to build mvs\dif files
rem rtlist - display mvs\dif dir in descending size order
erase %~dps0mvs\dif\*.* /q
call rtclean1 mvs\demo *
call rtclean1 mvs\test *
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
call rtcomp %~dps0mvs\demo %~dps0mvs\demo\save %~dps0mvs\dif
call rtcomp %~dps0mvs\test %~dps0mvs\test\save %~dps0mvs\dif
