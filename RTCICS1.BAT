rem run rtcics pgms and get cics\dif
erase %~dps0cics\dif\*.* /q
call RTCLEAN1 cics *
rem
rem asm statically linked subroutines called by cics macros
rem dfheai0 - alloc dsa
rem dfheai1 - free  dsa
rem
call RTC4 %~dps0cics\DFHEAI0 sysmac(cics+mac) systerm(RTCICS) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTC4 %~dps0cics\DFHEAI1 sysmac(cics+mac) systerm(RTCICS) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem asml cics command processor
rem
call RTC6 %~dps0cics\CICS          sysmac(cics+mac) systerm(RTCICS) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem asml cics programs
rem
call RTC6 %~dps0cics\TESTCIC1 cics sysmac(cics+mac) systerm(RTCICS) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTC6 %~dps0cics\TESTCIC2 cics sysmac(cics+mac) systerm(RTCICS) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTC6 %~dps0cics\TESTGUI4 cics sysmac(cics+mac) systerm(RTCICS) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTC6 %~dps0cics\TESTGUI6 cics sysmac(cics+mac) systerm(RTCICS) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTC6 %~dps0cics\TESTAPL1 cics sysmac(cics+mac) systerm(RTCICS) %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTCOMP %~dps0cics %~dps0cics\save %~dps0cics\dif
