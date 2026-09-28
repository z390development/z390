@if /I "%1" == "tron" (echo on) else (echo off)
rem assemble, link test programs and files for zvsam(2)

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

rem assemble and link utilities
rem -- zVSAM V2 introduces the OCPL to allow Open/Close of muiltiple files
rem -- but that code has not been written yet for sz390.java's open/close handling.
set ERRMSG=assembly/link error
call bat\asml vsam2\mlc\ZREPRO   zVSAM(1) stats sys390(linklib) || goto errmsg
call bat\asml vsam2\mlc\ZVSAM19C zVSAM(2) stats sys390(linklib) || goto errmsg
call bat\asml vsam2\mlc\Z390CAT2 zVSAM(2) stats                 || goto errmsg

set RC=0
goto return

:errmsg
echo Assembly failed: see messages above. Terminating zVSAM build.
set RC=8

:return
popd
exit /b %RC%
