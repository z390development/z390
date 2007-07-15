rem regression test z390 UTIL utilities
erase util\dif\*.* /q
rem build and test unref
rem build superzap and unref utilities
call util\unref\BUILD NOTIMING
call util\superzap\BUILD NOTIMING
rem run unref demo
call util\unref\UNREF UTIL\UNREF\UNREF NOTIMING
call RTCOMP util\unref    util\unref\save    util\dif
rem run superzap demos
call util\superzap\ZAPDEMO1 NOTIMING
call util\superzap\ZAPDEMO2 NOTIMING
call RTCOMP util\superzap util\superzap\save util\dif
rem end of RTUTIL1.BAT
