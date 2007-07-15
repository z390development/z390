rem run vse\test and vse\demo pgms and gen vse\dif
rem rt1 - assemble, link, and execute with notiming
rem rt6 - assemble and link with notiming
rem rtcomp - compare vse\test files with vse\save files to build vse\dif files
rem rtlist - display vse\dif dir in descending size order
erase %~dps0vse\dif\*.* /q
call RTCLEAN1 vse\demo *
call RTCLEAN1 vse\test *
rem
rem assemble, link, and execute demo wto 'hello world'
rem
call RT1 %~dps0vse\demo\DEMOVSE1 sysmac(vse\mac+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASML %~dps0demo\DEMOWTO1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0vse\demo\DEMOVSE2 sysmac(vse\mac+mac) sys390(vse\demo+demo) %1 %2 %3 %4 %5 %6 %7 %8 %9
erase vse\demo\DEMOVSE3.TF2
call RT5 %~dps0vse\demo\DEMOVSE3 sysmac(vse\mac+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem assemble common application macros as test (not executable pgm)
rem
call RT1 %~dps0vse\test\TESTVSE1 sysmac(vse\mac+mac) MEM(32) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTCOMP %~dps0vse\demo %~dps0vse\demo\save %~dps0vse\dif
call RTCOMP %~dps0vse\test %~dps0vse\test\save %~dps0vse\dif
