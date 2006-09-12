rem run rtlink pgms and gen linklib\dif
erase linklib\dif\*.* /q
call rtclean1 linklib *
call rt4 linklib\fpconvrt sysmac(mac+linklib)
call rt4 linklib\FPCONMFC sysmac(mac+linklib)
call rt4 linklib\CVTTOHEX 
call rt1 linklib\testfpc1 sysobj(linklib)
call rt1 linklib\testfpc2 sysobj(linklib)
rem test expansion of rtgendir and rtgencmp and copy gen'd files 
call rt3 linklib\rtgendir sysparm(rtgendir)
call rt3 linklib\rtgencmp sysparm(rtgencmp)
copy linklib\rtgen*.dat linklib\save
copy linklib\rtgen*.pch linklib\save
call rtcomp linklib linklib\save linklib\dif
rem erase rtgen difs since they are regen'd for each rtcomp
erase linklib\dif\rtgen*.dat
erase linklib\dif\rtgen*.pch
