rem regression test z390 GUI interactive programs in guam
call rt\RTCLEAN1 guam\demo *
call rt\RTCLEAN1 guam\test *
call Z390 /sc Z390.INI
call rt\RT1 guam\demo\DEMOAPL1  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 guam\demo\DEMOGUI1  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 guam\demo\DEMOGUI2  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 guam\demo\DEMOGUI3  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 guam\demo\DEMOGUI4  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 guam\demo\DEMOGUI5  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 guam\demo\DEMOGUI6  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 guam\demo\HELLO1  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 guam\demo\HELLO2  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 guam\demo\HELLO3  guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 guam\test\AID     guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 guam\test\TESTGP1 guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 guam\test\TESTGP2 guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 guam\test\TESTGP3 guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 guam\test\TESTGP4 guam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTCOMP guam\demo rt\guam\demo\save rt\guam\demo\dif
call rt\RTCOMP guam\test rt\guam\test\save rt\guam\test\dif
erase rt\guam\dif\*.* /q
copy  rt\guam\demo\dif\*.* rt\guam\dif
copy  rt\guam\test\dif\*.* rt\guam\dif
call rt\RTLIST rt\guam\dif
rem end of rtguam.bat
