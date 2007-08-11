rem regression test z390 SOA application generator and TCPIO sockets 
rem test soa\demo first using static link and exec
call soa\demo\DEMOSTD
rem test soa\demo again using SOA support to run services on server 
rem press enter to continue running client after server is started
call soa\demo\CLEANUP    
call soa\demo\DEMOSOA  %1 %2 %3 %4 %5 %6 %7 %8 %9
erase soa\dif\*.* /q
call RTCOMP soa\demo soa\demo\save soa\dif
rem test TCPIO client sending msg to echo server on specified port
rem press enter to continue running client after server is started
call  asml soa\test\TESTMSGS %1 %2 %3 %4 %5 %6 %7 %8 %9
start exec soa\test\TESTMSGS PARM(3900) trace con %1 %2 %3 %4 %5 %6 %7 %8 %9
call asmlg soa\test\TESTMSGC parm(*) CON %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTCOMP soa\test soa\test\save soa\dif
rem end of rtsoa.bat
