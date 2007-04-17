rem regression test z390 UTIL utilities
erase util\dif\*.* /q
reum build and test unref
call RTCLEAN1 util\unref *
call RTCLEAN1 util\superzap *
call util\unref\BUILD NOTIMING
call util\unref\UNREF UTIL\UNREF\UNREF NOTIMING
call RTCOMP util\unref    util\unref\save    util\dif
rem build and test superzap
call util\superzap\BUILD    NOTIMING
call util\superzap\ZAPDEMO1 NOTIMING
call util\superzap\ZAPDEMO2 NOTIMING
call RTCOMP util\superzap util\superzap\save util\dif
rem end of RTUTIL1.BAT
