rem run z390 regression tests
rem  1. generate test files from source test programs in test dirs 
rem  2. compare  test files with saved files and generate dif files
rem  3. list dif directory files descending by size to see any changes 
rem process stops on errors
REM ******************************************************************
REM * 07/10/07 RPI 652 ADD RTVSE1 AND RTUTIL1 TO TEST VSE AND UTILS
REM * 07/18/07 RPI 658 CHANGE RTCLEAN TO DIRCLEAN                   
REM * 01/22/08 RPI 716 MOVE ALL RT SAVE DIRECTORIES AND BAT FILES TO RT
REM ******************************************************************
erase rt\all\*.* /q 
call rt\DIRCLEAN
call rt\RTLINK1   systerm(linklib\rtlink) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTAST1    systerm(assist\rtast) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTDEMO1   systerm(demo\rtdemo) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTMFACC1  systerm(mfacc\rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTMVS1    systerm(mvs\test\rtmvs) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTVSAM1   systerm(vsam\test\rtvsam) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTVSE1    systerm(vse\test\rtvse) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTTEST1   systerm(rt\test\rttest) %1 %2 %3 %4 %5 %6 %7 %8 %9
copy rt\linklib\dif\*.*         rt\all
copy rt\assist\dif\*.*          rt\all
copy rt\demo\dif\*.*            rt\all
copy rt\mfacc\dif\*.*           rt\all
copy rt\mvs\demo\dif\*.*        rt\all
copy rt\mvs\test\dif\*.*        rt\all
copy rt\test\dif\*.*            rt\all
copy rt\vsam\demo\dif\*.*       rt\all
copy rt\vsam\test\dif\*.*       rt\all
copy rt\vse\demo\dif\*.*        rt\all
copy rt\vse\test\dif\*.*        rt\all
copy rt\linklib\dif\DIFF.TXT    rt\all\RTLINK.DIF
copy rt\assist\dif\DIFF.TXT     rt\all\RTAST.DIF
copy rt\demo\dif\DIFF.TXT       rt\all\RTDEMO.DIF
copy rt\mfacc\dif\DIFF.TXT      rt\all\RTNFACC.DIF
copy rt\mvs\dif\DIFF.TXT        rt\all\RTMVS.DIF
copy rt\vsam\test\dif\DIFF.TXT  rt\all\RTVSAM.DIF
copy rt\vse\test\dif\DIFF.TXT   rt\all\RTVSE.DIF
copy rt\test\dif\DIFF.TXT       rt\all\RTTEST.DIF
erase rt\all\RTGEN*.BAL
erase rt\all\RTGEN*.PCH
erase rt\all\RTGEN*.DAT
call  rt\RTLIST rt\all notime
rem end of rt.bat

