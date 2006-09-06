rem run test and demo regression tests 
rem process stops on errors
rem check temp\*.d?? difference files for changes from last test
erase %~dps0test\*.d*
erase %~dps0demo\*.d*
erase %~dps0cics\*.d*
erase %~dps0mvs\test\*.d*
erase %~dps0mvs\demo\*.d*
call %~dps0rttest %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0rtdemo %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0rtcics %1 %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0rtmvs  %1 %2 %3 %4 %5 %6 %7 %8 %9
erase %~dps0?z390.d*
erase %~dps0temp\*.d*
copy %~dps0demo\*.d*     temp
copy %~dps0test\*.d*     temp
copy %~dps0cics\*.d*     temp
copy %~dps0mvs\demo\*.d* temp
copy %~dps0mvs\test\*.d* temp
dir %~dps0temp\*.d*/o-s/p

