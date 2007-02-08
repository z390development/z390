rem regression test z390 SOA application generator
call soa\demo\CLEANUP    
call soa\demo\DEMOSOA  %1 %2 %3 %4 %5 %6 %7 %8 %9
erase soa\dif\*.* /q
call RTCOMP soa\demo soa\demo\save soa\dif
rem end of rtsoa.bat
