rem run z390 regression tests
rem  1. generate test files from source test programs in test dirs 
rem  2. compare  test files with saved files and generate dif files
rem  3. list dif directory files descending by size to see any changes 
rem process stops on errors
erase %~dps0rt\all\*.* /q 
erase %~dps0linklib\dif\*.* /q 
erase %~dps0demo\dif\*.* /q 
erase %~dps0cics\dif\*.* /q 
erase %~dps0mvs\dif\*.* /q 
erase %~dps0rt\dif\*.* /q 
call RTCLEAN
call %~dps0RTLINK1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0RTDEMO1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0RTCICS1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0RTMVS1  %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0RTTEST1 %1 %2 %3 %4 %5 %6 %7 %8 %9
copy %~dps0linklib\dif\*.*  %~dps0rt\all
copy %~dps0demo\dif\*.*     %~dps0rt\all
copy %~dps0cics\dif\*.*     %~dps0rt\all
copy %~dps0rt\dif\*.*       %~dps0rt\all
copy %~dps0mvs\dif\*.*      %~dps0rt\all
copy %~dps0linklib\dif\DIFF.TXT  %~dps0rt\all\RTLINK.DIF
copy %~dps0demo\dif\DIFF.TXT     %~dps0rt\all\RTDEMO.DIF
copy %~dps0cics\dif\DIFF.TXT     %~dps0rt\all\RTCICS.DIF
copy %~dps0mvs\dif\DIFF.TXT      %~dps0rt\all\RTMVS.DIF
copy %~dps0rt\dif\DIFF.TXT       %~dps0rt\all\RTTEST.DIF
erase %~dps0rt\all\RTGEN*.BAL
erase %~dps0rt\all\RTGEN*.PCH
erase %~dps0rt\all\RTGEN*.DAT
call RTLIST %~dps0rt\all 
rem end of rt.bat

