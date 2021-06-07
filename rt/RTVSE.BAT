rem run z390 vse regression tests using zse\mac macro library
rem rt1 - assemble, link, and execute with notiming
rem rt6 - assemble and link with notiming
rem rtcomp - compare mvs\test files with mvs\save files to build mvs\dif files
rem rtlist - display mvs\dif dir in descending size order
call rt\RTVSE1 systerm(vse\test\rtvse) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTLIST rt\vse\dif 
rem end of rtvse.bat
