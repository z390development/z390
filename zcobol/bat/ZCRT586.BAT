rem regression test zcobol i586 COBOL demos
rem run demo pgms and gen dif report for i586
echo off
rem
rem cbl to masm exe demos using Intel 586+ HLA and MASM language
rem
call ZCCLEAN1 zcobol\demo *
erase zcobol\demo\i586\*.* /q
call ZC586CLG zcobol\demo\hello    notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
copy zcobol\demo\hello.* zcobol\demo\i586
rem
rem compare generated files to saved files to create dif file with changes
rem
erase zcobol\demo\i586\dif\*.* /q
call RTCOMP zcobol\demo\i586 zcobol\demo\i586\save zcobol\demo\i586\dif
rem
rem move all dif files to single dir and list in descending size
rem
erase zcobol\all\*.* /q
copy zcobol\demo\i586\dif\*.* zcobol\all\*_di586.*
call RTLIST zcobol\all
rem end of zcrt586.bat
