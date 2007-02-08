rem regression test z390 GUI interactive programs in guam
call RTCLEAN1 guam\demo *
call RTCLEAN1 guam\test *
call Z390 /sc z390.ini
call RT1 %~dps0guam\demo\DEMOAPL1  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0guam\demo\DEMOGUI1  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0guam\demo\DEMOGUI2  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0guam\demo\DEMOGUI3  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0guam\demo\DEMOGUI4  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0guam\demo\DEMOGUI5  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0guam\demo\DEMOGUI6  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0guam\demo\HELLO1  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0guam\demo\HELLO2  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0guam\test\AID     guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0guam\test\TESTGP1 guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0guam\test\TESTGP2 guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0guam\test\TESTGP3 guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call RT1 %~dps0guam\test\TESTGP4 guam %1 %2 %3 %4 %5 %6 %7 %8 %9
erase guam\dif\*.* /q
call RTCOMP guam\demo guam\demo\save guam\dif
call RTCOMP guam\test guam\test\save guam\dif
call RTLIST guam\dif  %1
rem end of rtguam.bat
