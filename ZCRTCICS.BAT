REM TEST CICS CBL PGMS
call rt\RTCLEAN1 rt\cics\test *
erase rt\cics\dif\*.* /q
rem
CALL ZCRTCIC1 TESTBMC1  %1 %2 %3 %4 %5 %6 %7 %8 
CALL ZCRTCIC1 TESTICC1  %1 %2 %3 %4 %5 %6 %7 %8 
CALL ZCRTCIC1 TESTBEC1  %1 %2 %3 %4 %5 %6 %7 %8 
CALL ZCRTCIC1 TESTBEC2  %1 %2 %3 %4 %5 %6 %7 %8 
CALL ZCRTCIC1 TESTBEC4  %1 %2 %3 %4 %5 %6 %7 %8 
CALL ZCRTCIC1 TESTBEC5  %1 %2 %3 %4 %5 %6 %7 %8 
CALL ZCRTCIC1 TESTBEC6  %1 %2 %3 %4 %5 %6 %7 %8 
CALL ZCRTCIC1 TESTBEC7  %1 %2 %3 %4 %5 %6 %7 %8 
CALL ZCRTCIC1 TESTBEC8  %1 %2 %3 %4 %5 %6 %7 %8 
CALL ZCRTCIC1 TESTBEC9  %1 %2 %3 %4 %5 %6 %7 %8 
CALL ZCRTCIC1 TESTVSC1  %1 %2 %3 %4 %5 %6 %7 %8 
CALL ZCRTCIC1 TESTVSC2  %1 %2 %3 %4 %5 %6 %7 %8 
CALL ZCRTCIC1 TESTVSC3  %1 %2 %3 %4 %5 %6 %7 %8 
call rt\RTCOMP rt\cics\test rt\cics\test\save rt\cics\dif
call rt\RTLIST rt\cics\dif 
