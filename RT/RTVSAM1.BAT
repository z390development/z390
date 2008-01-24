rem run rtvsam pgms and gen rt\dif
call rt\RTCLEAN1 vsam\demo *
call rt\RTCLEAN1 vsam\test *
call vsam\test\BUILDCAT notiming
call vsam\test\ESF1TEST notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF1ERR1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF1SKP1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF2TEST notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF3TEST notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF3GEN1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV1TEST notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV1SKP1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV2TEST notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV3TEST notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV4TEST notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRF1TEST notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRF1SKP1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRV1TEST notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRV1SKP1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF1RPO1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\KSF1TEST notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF4RPO1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV1RPO1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV5RPO1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRF1RPO1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRF2RPO1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRV1RPO1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF1RPQ1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF4RPQ1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV1RPQ1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV5RPQ1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRF2RPQ1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRV1RPQ1 notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTCOMP vsam\test rt\vsam\test\save rt\vsam\test\dif
REM asml ESF1.390 which is not a catalog to verify ESF1.TF? works ok
call asml vsam\demo\ESF1 notiming stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\demo\demos notiming tracev stats %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTCOMP vsam\demo rt\vsam\demo\save rt\vsam\demo\dif
erase rt\vsam\dif\*.* /q
copy  rt\vsam\demo\dif\*.* rt\vsam\dif
copy  rt\vsam\test\dif\*.* rt\vsam\dif
