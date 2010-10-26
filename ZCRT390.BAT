rem regression test zcobol z390 COBOL demos
rem run demo pgms and gen dif report for z390
echo off
rem
rem cbl to 390 demos using z390 portable mainframe assembler language
rem
call ZCCLEAN1 zcobol\demo *
erase zcobol\demo\z390\*.* /q
call ZC390CLG zcobol\demo\hello    notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call ZC390CLG zcobol\demo\datetime notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
erase zcobol\demo\copyfile.out
call ZC390CLG zcobol\demo\copyfile notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call ZC390CLG zcobol\demo\POWERS   notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
copy zcobol\demo\*.* zcobol\demo\z390
rem
rem compare generated files to saved files to create dif file with changes
rem
erase zcobol\demo\z390\dif\*.* /q
call RTCOMP zcobol\demo\z390 zcobol\demo\z390\save zcobol\demo\z390\dif
rem
rem move all dif files to single dir and list in descending size
rem
erase zcobol\all\*.* /q
copy zcobol\demo\z390\dif\*.* zcobol\all\*_dz390.*
call RTLIST zcobol\all
rem end of rt\zcrt390.bat
