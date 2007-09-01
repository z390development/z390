rem run rtvsam pgms and gen rt\dif
erase %~dps0vsam\test\dif\*.* /q
call RTCLEAN1 vsam\test *
REM test ESDS fixed length
call vsam\test\ESF1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF1REPO notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test ESDS variable length - 3 records with 100, 200, 300 chars
call vsam\test\ESV1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV1REPO notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test ESDS variable length - 6 records from 1 to 100,000
call vsam\test\ESV2TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test RRDS fixed length = 3 80 byte records at 0, 4, 2
call vsam\test\RRF1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test RRDS variable length = 100, 400, 200 at 0, 4, 2
call vsam\test\RRV1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTCOMP %~dps0vsam\test %~dps0vsam\test\save %~dps0vsam\test\dif
erase %~dps0vsam\demo\dif\*.* /q
call RTCLEAN1 vsam\demo * 
REM test ESDS fixed length
call vsam\demo\demos notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
