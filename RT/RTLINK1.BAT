erase rt\linklib\dif\*.* /q
call rt\RTCLEAN1 linklib *
erase linklib\REPRO.LOG
erase linklib\REPRO.STA
erase linklib\REPRO.TRE
call rt\rt6 linklib\REPRO %1 %2 %3 %4 %5 %6 %7 %8 %9   
call rt\rt6 linklib\SUPERZAP %1 %2 %3 %4 %5 %6 %7 %8 %9
call linklib\ZAPDEMO1 stats  notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call linklib\ZAPDEMO2 stats  notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\rt6 linklib\UNREF ASCII SYSCPY(MAC+LINKLIB) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT4 linklib\FPCONVRT sysmac(mac+linklib)    %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT4 linklib\FPCONMFC sysmac(mac+linklib)    %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT4 linklib\CVTTOHEX %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT4 linklib\SQXTR    %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT4 linklib\DAT      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 linklib\IEFBR14  sysobj(linklib) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 linklib\TESTFPC1 sysobj(linklib) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 linklib\TESTFPC2 sysobj(linklib) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem gen zstrmac translator using zstrmac.bat 
call zstrmac linklib\zstrmac.zsm linklib\zstrmac.txt notiming stats
rem verify zstrmac.txt matches linklib\zstrmac2.sav
fc zstrmac.txt linklib\zstrmac.sav > linklib\zstrmac.dif
rem test expansion of rtgendir and rtgencmp and copy gen'd files 
call rt\RT3 linklib\RTGENDIR sysparm(RTGENDIR) notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT3 linklib\RTGENCMP sysparm(RTGENCMP) notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
erase rt\linklib\save\repro.log
erase rt\linklib\save\repro.tre
call  rt\RTCOMP linklib rt\linklib\save rt\linklib\dif
