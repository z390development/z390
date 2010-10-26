rem regression test zcobol java COBOL demos
rem run demo pgms and gen dif report for java
rem note zcjavclg uses explicit path to j2se jdk for javac compiler
echo off
rem
rem cbl to java class demos using J2SE Portable Java language           
rem
call ZCCLEAN1 zcobol\demo *
erase zcobol\demo\java\*.* /q
call ZCJAVCLG zcobol\demo\HELLO    notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
copy zcobol\demo\hello.* zcobol\demo\java
rem
rem
rem compare generated files to saved files to create dif file with changes
rem
erase zcobol\demo\java\dif\*.* /q
call RTCOMP zcobol\demo\java zcobol\demo\java\save zcobol\demo\java\dif
rem
rem move all dif files to single dir and list in descending size
rem
erase zcobol\all\*.* /q
copy zcobol\demo\java\dif\*.* zcobol\all\*_djava.*
call RTLIST zcobol\all
rem end of zcrtjav.bat
