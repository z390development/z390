rem assemble and link and verify linklib programs
call rt\RTLINK1 systerm(linklib\rtlink) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTLIST rt\linklib\dif
rem end of RTLIST.bat
