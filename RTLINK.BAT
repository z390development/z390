rem assemble and link and verify linklib programs
call rt6 linklib\fpconvrt sysmac(mac+linklib)
call rt6 linklib\FPCONMFC sysmac(mac+linklib)
call rt6 linklib\CVTTOHEX 
call rt1 linklib\testfpc1 sysobj(linklib)
call rt1 linklib\testfpc2 sysobj(linklib)
