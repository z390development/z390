rem regression test z390 GUI interactive programs in guam
call rtclean1 guam\demo *
call rtclean1 guam\test *
call z390 /sc z390.ini
call rt1 %~dps0guam\demo\demoapl1  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0guam\demo\demogui1  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0guam\demo\demogui2  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0guam\demo\demogui3  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0guam\demo\demogui4  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0guam\demo\demogui5  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0guam\demo\demogui6  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0guam\demo\hello1  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0guam\demo\hello2  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0guam\test\aid     guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0guam\test\testgp1 guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0guam\test\testgp2 guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0guam\test\testgp3 guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt1 %~dps0guam\test\testgp4 guam %1 %2 %3 %4 %5 %6 %7 %8 %9
erase guam\dif\*.* /q
call rtcomp guam\demo guam\demo\save guam\dif
call rtcomp guam\test guam\test\save guam\dif
call rtlist guam\dif  %1
rem end of rtguam.bat
