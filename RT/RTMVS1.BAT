rem run mvs\test and mvs\demo pgms and gen mvs\dif
rem after installing z390, you can download MVS\MVSLIB from www.z390.org
rem rt1 - assemble, link, and execute with notiming
rem rt6 - assemble and link with notiming
rem rtcomp - compare mvs\test files with mvs\save files to build mvs\dif files
rem rtlist - display mvs\dif dir in descending size order
call rt\RTCLEAN1 mvs\demo *
call rt\RTCLEAN1 mvs\test *
rem
rem assemble, link, and execute demo wto 'hello world'
rem
call rt\RT1 mvs\demo\DEMO sysmac(mvs\maclib+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem assemble common application macros as test (not executable pgm)
rem
call rt\RT6 mvs\test\TESTMVS1 sysmac(mvs\maclib+mac) objhex %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT6 mvs\test\TESTCVTX sysmac(mvs\maclib+mac) objhex %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT6 mvs\test\TESTCVT1 sysmac(mvs\maclib+mac) objhex %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTCOMP mvs\demo rt\mvs\demo\save rt\mvs\demo\dif
call rt\RTCOMP mvs\test rt\mvs\test\save rt\mvs\test\dif
erase rt\mvs\dif\*.* /q
copy  rt\mvs\demo\dif\*.* rt\mvs\dif
copy  rt\mvs\test\dif\*.* rt\mvs\dif
