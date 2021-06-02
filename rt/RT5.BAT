REM test and verify file i/o from sysut1=%1.tf1 and sysut2=%2.tf2
set SYSUT1=%1.TF1
set SYSUT2=%1.TF2
set SYSOUT=%1.TF3
call ASMLG %1 bal notiming stats %2 %3 %4 %5 %6 %7 %8 %9
