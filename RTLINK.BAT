rem assemble and link and verify linklib programs
call %~dps0rtlink1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0rtlist %~dps0linklib\dif
rem end of rtlink.bat
