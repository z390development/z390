erase linklib\dif\*.* /q
call rt\RTCLEAN1 linklib *
erase linklib\REPRO.LOG
erase linklib\REPRO.STA
erase linklib\REPRO.TRE
call rt\rt6 linklib\REPRO    
call rt\rt6 linklib\SUPERZAP 
call linklib\ZAPDEMO1 stats
call linklib\ZAPDEMO2 stats
call rt\rt6 linklib\UNREF ASCII SYSCPY(MAC+LINKLIB)
call rt\RT4 linklib\FPCONVRT sysmac(mac+linklib)
call rt\RT4 linklib\FPCONMFC sysmac(mac+linklib)
call rt\RT4 linklib\CVTTOHEX 
call rt\RT1 linklib\TESTFPC1 sysobj(linklib)
call rt\RT1 linklib\TESTFPC2 sysobj(linklib) 
rem test expansion of rtgendir and rtgencmp and copy gen'd files 
call rt\RT3 linklib\RTGENDIR sysparm(RTGENDIR) notiming 
call rt\RT3 linklib\RTGENCMP sysparm(RTGENCMP) notiming 
copy linklib\RTGEN*.DAT rt\linklib\save
copy linklib\RTGEN*.PCH rt\linklib\save
copy linklib\RTGEN*.STA rt\linklib\save
erase rt\linklib\save\repro.log
erase rt\linklib\save\repro.tre
call  rt\RTCOMP linklib rt\linklib\save rt\linklib\dif
rem erase rtgen difs since they are regen'd for each rtcomp
erase rt\linklib\dif\RTGEN*.dat
erase rt\linklib\dif\RTGEN*.pch
