rem run z390 regression tests
rem  1. generate test files from source test programs in test dirs 
rem  2. compare  test files with saved files and generate dif files
rem  3. list dif directory files descending by size to see any changes 
rem process stops on errors
erase %~dps0rt\all\*.* /q 
call rtclean
call %~dps0rtlink1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0rtdemo1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0rtcics1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0rtmvs1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0rtsoa1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0rttest1 %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0linklib\dif\*.*  %~dps0rt\all
copy %~dps0demo\dif\*.*     %~dps0rt\all
copy %~dps0cics\dif\*.*     %~dps0rt\all
copy %~dps0rt\dif\*.*       %~dps0rt\all
copy %~dps0mvs\dif\*.*      %~dps0rt\all
copy %~dps0soa\dif\*.*      %~dps0rt\all
erase %~dps0rt\all\rtgen*.bal
erase %~dps0rt\all\rtgen*.pch
erase %~dps0rt\all\rtgen*.dat
call rtlist %~dps0rt\all 
rem end of rt.bat

