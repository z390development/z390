@if /I "%1" == "tron" (echo on) else (echo off)
rem Script to create z390 distribution zip file

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

rem asml routines
set z_ReturnCode=0
call bat\asm  barcode\encodetr sysmac(+barcode) syscpy(+barcode) %1 %2 %3 %4 || goto error
call bat\asml barcode\s4bcdrva sysmac(+barcode) syscpy(+barcode) %1 %2 %3 %4 || goto error
copy barcode\*.390 linklib                                                   || goto error
goto exit

:error
set z_ReturnCode=8

:exit
rem ----- Lvl(%z_NestLevel%) End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
popd
exit /b %z_ReturnCode%