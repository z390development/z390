rem run rtvsam pgms and gen rt\dif
erase %~dps0vsam\test\dif\*.* /q
call RTCLEAN1 vsam\test *
REM test ESDS fixed length
call vsam\test\ESF1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF2TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF1REPO notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF2TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF3TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test ESDS var len SEQ - 3 records 100, 200, 300 chars
call vsam\test\ESV1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV1REPO notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test ESDS variable length - 6 records from 1 to 100,000
call vsam\test\ESV2TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test ESDS var len  SEQ,ADR,UPD,RBA,XRBA - 3 records 100,200, 300
call vsam\test\ESV3TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test ESDS var len  SEQ,BWD,LRD create and read backwards
call vsam\test\ESV4TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test RRDS fixed length = 3 80 byte records at 0, 4, 2
call vsam\test\RRF1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test RRDS variable length = 100, 400, 200 at 0, 4, 2
call vsam\test\RRV1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTCOMP %~dps0vsam\test %~dps0vsam\test\save %~dps0vsam\test\dif
erase %~dps0vsam\demo\dif\*.* /q
call RTCLEAN1 vsam\demo * 
REM test ESDS fixed length
call vsam\demo\demos notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
