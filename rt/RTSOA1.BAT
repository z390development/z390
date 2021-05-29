rem regression test z390 SOA application generator and TCPIO sockets 
rem test soa\demo first using static link and exec
call rt\RTCLEAN1 soa\demo *
call rt\RTCLEAN1 soa\test *
call soa\demo\DEMOSTD
rem test soa\demo again using SOA support to run services on server 
rem press enter to continue running client after server is started
call soa\demo\CLEANUP    
call soa\demo\DEMOSOA stats  %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTCOMP soa\demo rt\soa\demo\save rt\soa\demo\dif
rem test TCPIO client sending msg to echo server on specified port
rem press enter to continue running client after server is started
call  asml soa\test\TESTMSGS stats %1 %2 %3 %4 %5 %6 %7 %8 %9
start exec soa\test\TESTMSGS PARM(3900) trace con stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call  asmlg soa\test\TESTMSGC parm(*) CON stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call  rt\RTCOMP soa\test rt\soa\test\save rt\soa\test\dif
erase rt\soa\dif\*.* /q
copy  rt\soa\demo\dif\*.* rt\soa\dif
copy  rt\soa\test\dif\*.* rt\soa\dif
rem end of rtsoa.bat
