rem run z390 zcobol sample regression tests
@setlocal
@pushd %~dp0
cd..
cd..
CALL BAT\CBLCLG ZCOBOL\tests\testadd1 %1 %2 %3 %4 %5 %6 %7 %8 %9
CALL BAT\CBLCLG ZCOBOL\tests\testadd2 %1 %2 %3 %4 %5 %6 %7 %8 %9
CALL BAT\CBLCLG ZCOBOL\tests\testif1  %1 %2 %3 %4 %5 %6 %7 %8 %9
CALL BAT\CBLCLG ZCOBOL\tests\testif2  %1 %2 %3 %4 %5 %6 %7 %8 %9
CALL BAT\CBLCLG ZCOBOL\tests\testif3  %1 %2 %3 %4 %5 %6 %7 %8 %9
CALL BAT\CBLCLG ZCOBOL\tests\testmov1 %1 %2 %3 %4 %5 %6 %7 %8 %9 
CALL BAT\CBLClG ZCOBOL\tests\testmov2 %1 %2 %3 %4 %5 %6 %7 %8 %9
CALL BAT\CBLCLG ZCOBOL\tests\testmov3 %1 %2 %3 %4 %5 %6 %7 %8 %9
@REM pause verify ZCOBOL regression tests