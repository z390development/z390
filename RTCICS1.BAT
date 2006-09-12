rem run rtcics pgms and get cics\dif
erase %~dps0cics\dif\*.* /q
call rtclean1 cics *
rem
rem asm statically linked subroutines called by cics macros
rem dfheai0 - alloc dsa
rem dfheai1 - free  dsa
rem
call rtc4 %~dps0cics\dfheai0 sysmac(cics+mac) systerm(rtcics) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rtc4 %~dps0cics\dfheai1 sysmac(cics+mac) systerm(rtcics) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem asml cics command processor
rem
call rtc6 %~dps0cics\cics          sysmac(cics+mac) systerm(rtcics) %1 %2 %3 %4 %5 %6 %7 %8 %9
rem
rem asml cics programs
rem
call rtc6 %~dps0cics\testcic1 cics sysmac(cics+mac) systerm(rtcics) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rtc6 %~dps0cics\testcic2 cics sysmac(cics+mac) systerm(rtcics) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rtc6 %~dps0cics\testgui4 cics sysmac(cics+mac) systerm(rtcics) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rtc6 %~dps0cics\testgui6 cics sysmac(cics+mac) systerm(rtcics) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rtc6 %~dps0cics\testapl1 cics sysmac(cics+mac) systerm(rtcics) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rtcomp %~dps0cics %~dps0cics\save %~dps0cics\dif
