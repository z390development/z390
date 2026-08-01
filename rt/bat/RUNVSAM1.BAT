@if /I "%1" == "tron" (echo on) else (echo off)
rem rebuild and test sort utility

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

pushd %~dps0..\..

rem
rem build the catalog
call bat\asml  %z_TraceMode% rt\vsam\BLDCAT1  @rt\vsam\RTVSAM1.opt || goto error

rem
rem create the ESDS_F VES file
set myesds=rt\vsam\BLDCAT1.ESDS_F
call bat\asm   %z_TraceMode% rt\vsam\BLDESDS0 @rt\vsam\RTVSAM0.opt
     if %ERRORLEVEL% NEQ 15 goto error
call bat\asmlg %z_TraceMode% rt\vsam\BLDESDS0 @rt\vsam\RTVSAM1.opt || goto error

rem if we get here, there were no errors
echo Verify all zVSAM(1) tests ok
goto return

:error
echo %0 ERROR: Test program failed. See log and listings for details.
set z_ReturnCode=8

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
