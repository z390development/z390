cd..
rem test DFP instructions
call bat\asmlg tests\testdfp1 traceall sysmac(mac) optable(z390)
pause verify testdfp1 rc=0
