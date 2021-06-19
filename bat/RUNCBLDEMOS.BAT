@echo off
rem run z390 zcobol demos hello world, powers, copyfile, compute
setlocal
pushd %~dp0
cd..
CALL BAT\CBLCLG ZCOBOL\DEMO\HELLO   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
CALL BAT\CBLCLG ZCOBOL\DEMO\DATETIME %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
CALL BAT\CBLCLG ZCOBOL\DEMO\POWERS  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
CALL BAT\CBLCLG ZCOBOL\DEMO\COPYFILE %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
CALL BAT\CBLC  ZCOBOL\DEMO\COMPSUM  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
CALL BAT\ASMLG ZCOBOL\DEMO\CALLCOMP SYS390(ZCOBOL\LIB) %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error

goto:eof

:error
echo RUNCBLDE.ERROR: Encountered RC %ERRORLEVEL% - exiting
exit /b %ERRORLEVEL%
