@echo off
echo run z390 zcobol sample regression tests
setlocal
pushd %~dp0
cd..
cd..
CALL BAT\CBLCLG ZCOBOL\tests\testadd1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
CALL BAT\CBLCLG ZCOBOL\tests\testadd2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
CALL BAT\CBLCLG ZCOBOL\tests\testif1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
CALL BAT\CBLCLG ZCOBOL\tests\testif2  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
CALL BAT\CBLCLG ZCOBOL\tests\testif3  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
CALL BAT\CBLCLG ZCOBOL\tests\testmov1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
CALL BAT\CBLClG ZCOBOL\tests\testmov2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
CALL BAT\CBLCLG ZCOBOL\tests\testmov3 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error

goto:eof

:error
echo RUNCBLTE.ERROR: Encountered RC %ERRORLEVEL% - exiting
exit /b %ERRORLEVEL%
