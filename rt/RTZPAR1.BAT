rem run zpar and gen rt\zpar\dif
rem rtlist - display rt\zpar\dif dir in descending size order
call rt\RTCLEAN1 rt\zpar *
erase rt\zpar\dif\*.* /q
rem
rem generate zpar reports from saved TRE files
rem
copy rt\zpar\*.sav rt\zpar\*.TRE
call zpar\zpartra rt\zpar\DEMO  notime 
call zpar\zpartra rt\zpar\DEMOD INCLUDE(DEMO) DETAIL notime 
call zpar\zpartrc rt\zpar\hello notime 
call zpar\zpartrc rt\zpar\HELLOD INCLUDE(HELLO) DETAIL notime 
call zpar\zpartrs rt\zpar\TESTGUI6 include(testgui6) notime nomsg 
call zpar\zpartrs rt\zpar\TESTBEC1 include(testbec1) notime nomsg 
call rt\RTCOMP rt\zpar rt\zpar\save rt\zpar\dif
