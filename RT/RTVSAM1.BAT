rem run rtvsam pgms and gen rt\dif
erase %~dps0vsam\test\dif\*.* /q
call RTCLEAN1 vsam\test *
call vsam\test\BUILDCAT notiming
call vsam\test\ESF1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF1ERR1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF1SKP1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF2TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF3TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF3GEN1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV1SKP1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV2TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV3TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV4TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRF1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRF1SKP1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRV1TEST notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRV1SKP1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF1RPO1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF4RPO1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV1RPO1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV5RPO1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRF1RPO1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRF2RPO1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRV1RPO1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF1RPQ1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF4RPQ1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV1RPQ1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV5RPQ1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRF2RPQ1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRV1RPQ1 notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTCOMP %~dps0vsam\test %~dps0vsam\test\save %~dps0vsam\test\dif
erase %~dps0vsam\demo\dif\*.* /q
call RTCLEAN1 vsam\demo * 
REM asml ESF1.390 which is not a catalog to verify ESF1.TF? works ok
call asml vsam\demo\ESF1 %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\demo\demos notiming tracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTCOMP %~dps0vsam\demo %~dps0vsam\demo\save %~dps0vsam\demo\dif
