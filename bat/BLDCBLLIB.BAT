@if /I "%1" == "tron" (echo on) else (echo off)
rem rebuild zcobol\z390\zc390lib.390

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
call bat\asm  %z_TraceMode% zcobol\lib\ABORT    @zcobol\lib\ZCOBOL.OPT %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asm  %z_TraceMode% zcobol\lib\ACCEPT   @zcobol\lib\ZCOBOL.OPT %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asm  %z_TraceMode% zcobol\lib\CVTTOHEX @zcobol\lib\ZCOBOL.OPT %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asm  %z_TraceMode% zcobol\lib\DISPLAY  @zcobol\lib\ZCOBOL.OPT %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asm  %z_TraceMode% zcobol\lib\INSPECT  @zcobol\lib\ZCOBOL.OPT %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asm  %z_TraceMode% zcobol\lib\ZC390NUC @zcobol\lib\ZCOBOL.OPT %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% zcobol\lib\ZC390LIB @zcobol\lib\ZCOBOL.OPT %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
if not exist zcobol\lib\ZC390LIB.390 (set z_ReturnCode=12
                                      echo %0 ERROR: ZC390LIB.390 was not created
                                      goto return)
set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
