rem run rtvsam pgms and gen rt\dif
erase %~dps0vsam\test\dif\*.* /q
call RTCLEAN1 vsam\test *
REM build catalog
call ASML vsam\test\TESTCAT notiming
REM test ESDS fixed length
REM test creation of ESF1 from QSAM fixed, SEQ/ADR access
call vsam\test\ESF1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test point error on fixed ESF1 
call vsam\test\ESF1ERR1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test reading fixed ESF1 forward/backward starting at XRBA addr rec
call vsam\test\ESF1SKP1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test creation of ESF2 from QSAM fixed, SEQ and random UPD in place
call vsam\test\ESF2TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test load and unload of fixed ESDS with REPRO                       
call vsam\test\ESF1REPO notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test create and seq FWD and BWD access                                 
call vsam\test\ESF3TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test create and seq FWD and BWD access uisng GENCB for ACB/RPL         
call vsam\test\ESF3GEN1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test ESDS var len SEQ - 3 records 100, 200, 300 chars
REM test load and unload of fixed ESDS with REPRO using QSAM FT         
call vsam\test\ESF4REPO notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM create 3 var len rcd QSAM, load ESV1, verify by RBA/XRBA
call vsam\test\ESV1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test reading var ESV1 forward/backward starting at XRBA addr rec
call vsam\test\ESV1SKP1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test load and unload of variable ESDS with REPRO                       
call vsam\test\ESV1REPO notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test ESDS variable length - 6 records from 1 to 100,000
call vsam\test\ESV2TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test ESDS var len  SEQ,ADR,UPD,RBA,XRBA - 3 records 100,200, 300
call vsam\test\ESV3TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test ESDS var len  SEQ,BWD,LRD create and read backwards
call vsam\test\ESV4TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test RRDS fixed length = 3 80 byte records at 0, 4, 2
REM test load and unload of variable ESDS with REPRO using QSAM VT         
call vsam\test\ESV5REPO notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test RRDS fixed length records # 0, 4, and 2 random write/read
call vsam\test\RRF1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test RRDS fixed length records # 0, 4, and 2 point, seq fwd/bwd
call vsam\test\RRF1SKP1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test RRDS variable length = 100, 400, 200 at 0, 4, 2 random w/r
call vsam\test\RRV1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
REM test RRDS variable length = 100, 400, 200 at 0, 4, 2 point, seq F/B
call vsam\test\RRV1SKP1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTCOMP %~dps0vsam\test %~dps0vsam\test\save %~dps0vsam\test\dif
erase %~dps0vsam\demo\dif\*.* /q
call RTCLEAN1 vsam\demo * 
REM asml ESF1.390 which is not a catalog to verify ESF1.TF? works ok
call asml vsam\demo\ESF1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\demo\demos notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTCOMP %~dps0vsam\demo %~dps0vsam\demo\save %~dps0vsam\demo\dif
