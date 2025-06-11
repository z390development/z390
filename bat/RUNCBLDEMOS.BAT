@if /I "%1" == "tron" (echo on) else (echo off)
rem run z390 zcobol demos hello world, powers, copyfile, compute

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

pushd %~dps0..

set DEMOPATH=zcobol\demo

call bat\CBLCLG %z_TraceMode% %DEMOPATH%\HELLO.CBL    %1 %2 %3 %4 %5 %6 %7 %8 %9                || goto error
call bat\CBLCLG %z_TraceMode% %DEMOPATH%\HELLO2.Cobol %1 %2 %3 %4 %5 %6 %7 %8 %9                || goto error
call bat\CBLCLG %z_TraceMode% %DEMOPATH%\DATETIME     %1 %2 %3 %4 %5 %6 %7 %8 %9                || goto error
call bat\CBLCLG %z_TraceMode% %DEMOPATH%\POWERS       %1 %2 %3 %4 %5 %6 %7 %8 %9                || goto error

set INFILE=%DEMOPATH%\COPYFILE.IN
set OUTFILE=%DEMOPATH%\COPYFILE.OUT

call bat\CBLCLG %z_TraceMode% %DEMOPATH%\COPYFILE     %1 %2 %3 %4 %5 %6 %7 %8 %9                || goto error
call bat\CBLC   %z_TraceMode% %DEMOPATH%\COMPSUM      %1 %2 %3 %4 %5 %6 %7 %8 %9                || goto error
call bat\ASMLG  %z_TraceMode% %DEMOPATH%\CALLCOMP SYS390(zcobol\lib) %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
