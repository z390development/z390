rem run rtvsam pgms and gen rt\dif
erase rt\vsam\test\dif\*.* /q
erase rt\vsam\demo\dif\*.* /q
call rt\RTCLEAN1 vsam\demo *
call rt\RTCLEAN1 vsam\test *
erase rt\vsam\demo\dif\*.* /q
erase rt\vsam\test\dif\*.* /q
REN run vsam demos
REM asml ESF1.390 which is not a catalog to verify ESF1.TF? works ok
call asml vsam\demo\ESF1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\demo\demos @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTCOMP vsam\demo rt\vsam\demo\save rt\vsam\demo\dif
REM run vsam tests
call vsam\test\BUILDCAT @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF1TEST @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF1ERR1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF1SKP1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF2TEST @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF3TEST @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF3GEN1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV1TEST @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV1SKP1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV2TEST @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV3TEST @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV4TEST @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRF1TEST @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRF1SKP1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRV1TEST @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRV1SKP1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF1RPO1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\KSF1TEST @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\KSF2TST1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\KSF2TST2 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\KSF2TST3 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\KSF2TST4 @vsam\rtvsam notracev %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF4RPO1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV1RPO1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV5RPO1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRF1RPO1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRF2RPO1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRV1RPO1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF1RPQ1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESF4RPQ1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV1RPQ1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\ESV5RPQ1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRF2RPQ1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call vsam\test\RRV1RPQ1 @vsam\rtvsam %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTCOMP vsam\test rt\vsam\test\save rt\vsam\test\dif
erase rt\vsam\dif\*.* /q
copy  rt\vsam\demo\dif\*.* rt\vsam\dif
copy  rt\vsam\test\dif\*.* rt\vsam\dif
