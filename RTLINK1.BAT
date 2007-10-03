rem run rtlink pgms and gen linklib\dif
erase linklib\dif\*.* /q
call RTCLEAN1 linklib *
ERASE LINKLIB\REPRO.LOG
ERASE LINKLIB\REPRO.TRE
call rt6 linklib\REPRO    
call rt6 linklib\SUPERZAP 
call linklib\ZAPDEMO1 
call linklib\ZAPDEMO2 
call rt6 linklib\UNREF ASCII SYSCPY(MAC+LINKLIB)
call RT4 linklib\FPCONVRT sysmac(mac+linklib)
call RT4 linklib\FPCONMFC sysmac(mac+linklib)
call RT4 linklib\CVTTOHEX 
call RT1 linklib\TESTFPC1 sysobj(linklib)
call RT1 linklib\TESTFPC2 sysobj(linklib)
rem test expansion of rtgendir and rtgencmp and copy gen'd files 
call RT3 linklib\RTGENDIR sysparm(RTGENDIR)
call RT3 linklib\RTGENCMP sysparm(RTGENCMP)
copy linklib\RTGEN*.DAT linklib\save
copy linklib\RTGEN*.PCH linklib\save
erase linklib\save\repro.log
erase linklib\save\repro.tre
call RTCOMP linklib linklib\save linklib\dif
rem erase rtgen difs since they are regen'd for each rtcomp
erase linklib\dif\RTGEN*.dat
erase linklib\dif\RTGEN*.pch
