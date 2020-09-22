cd..
rem regression test z390 instructions
call bat\asmlg tests\testins2 trace sysmac(mac)
call bat\asmlg tests\testins3 trace sysmac(mac)
call bat\asmlg tests\testins4 trace sysmac(mac)
call bat\asmlg tests\testins5 trace sysmac(mac) optable(z390)
call bat\asmlg tests\testins6 trace sysmac(mac) syscpy(tests) optable(z390) TRACE
pause verify tests ran without errors