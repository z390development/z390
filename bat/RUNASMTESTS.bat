cd..
rem regression test z390 instructions
call bat\asmlg tests\testins2 trace sysmac(mac)
call bat\asmlg tests\testins3 trace sysmac(mac)
call bat\asmlg tests\testins4 trace sysmac(mac)
call bat\asmlg tests\testins5 trace sysmac(mac) optable(z390)
pause verify tests ran without errors