REM test and verify file i/o from sysut1=%1.tf1 and sysut2=%2.tf2
set sysut1=%1.tf1
set sysut2=%1.tf2
set sysout=%1.tf3
call %~dps0asmlg %1 bal notiming %2 %3 %4 %5 %6 %7 %8 %9
