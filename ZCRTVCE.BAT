rem regression test zcobol vce COBOL demos
rem run demo pgms and gen dif report for vce
echo off
rem
rem cbl to C exe demos using MS Visual C++ Express language
rem
call ZCCLEAN1 zcobol\demo *
erase zcobol\demo\vce\*.* /q
call ZCVCECLG zcobol\demo\hello    notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
copy zcobol\demo\hello.* zcobol\demo\vce
rem
rem
rem compare generated files to saved files to create dif file with changes
rem
erase zcobol\demo\vce\dif\*.* /q
call RTCOMP zcobol\demo\vce  zcobol\demo\vce\save  zcobol\demo\vce\dif
rem
rem move all dif files to single dir and list in descending size
rem
erase zcobol\all\*.* /q
copy zcobol\demo\vce\dif\*.*  zcobol\all\*_dvce.*
call RTLIST zcobol\all
rem end of zcrtvce.bat
