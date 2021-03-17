cd..
rem test RPI 2015 fix to IHASDWA 
call bat\asmlg rpi\tdp.mlc traceall
call bat\asmlg rpi\tmp.mlc traceall
pause verify test ran without errors