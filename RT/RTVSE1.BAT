rem run vse\test and vse\demo pgms and gen vse\dif
rem rt1 - assemble, link, and execute with notiming
rem rt6 - assemble and link with notiming
rem rtcomp - compare vse\test files with vse\save files to build vse\dif files
rem rtlist - display vse\dif dir in descending size order
erase rt\vse\dif\*.* /q
call rt\RTCLEAN1 vse\demo *
call rt\RTCLEAN1 vse\test *
rem
rem assemble, link, and execute demo wto 'hello world'
rem
call rt\RT1 vse\demo\DEMOVSE1 sysmac(vse\mac+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 vse\demo\DEMOVSE2 sysmac(vse\mac+mac) sys390(vse\demo+demo) %1 %2 %3 %4 %5 %6 %7 %8 %9
erase vse\demo\DEMOVSE3.TF2
call rt\RT5 vse\demo\DEMOVSE3 sysmac(vse\mac+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem assemble common application macros as test (not executable pgm)
rem
call rt\RT1 vse\test\TESTVSE1 sysmac(vse\mac+mac) MEM(32) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTCOMP vse\demo rt\vse\demo\save rt\vse\demo\dif
call rt\RTCOMP vse\test rt\vse\test\save rt\vse\test\dif
copy rt\vse\demo\dif\*.*  rt\vse\dif
copy rt\vse\test\dif\*.*  rt\vse\dif
