rem run z390 mvs 3.8 regression tests using MVS\MVSLIB macro library
rem after installing z390, you can download MVS\MVSLIB from www.z390.org
rem rt1 - assemble, link, and execute with notiming
rem rt6 - assemble and link with notiming
rem rtcomp - compare mvs\test files with mvs\save files to build mvs\dif files
rem rtlist - display mvs\dif dir in descending size order
call rt\RTMVS1 systerm(mvs\test\rtmvs) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTLIST rt\mvs\dif 
rem end of rtmvs.bat
