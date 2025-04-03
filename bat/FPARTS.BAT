@if /I "%1" == "tron" (echo on) else (echo off)
rem extract file path parts from complete file designation

rem setlocal --> cannot do this because we need to return environment variables
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

set z_ReturnCode=0
if "%1" == "/?" goto help
if "%1" ==  "?" goto help
if not exist %1 set z_ReturnCode=8

set z_fdrv=%~d1
set z_fpath=%~p1
set z_fname=%~n1
set z_fext=%~x1
:return
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
set /A z_NestLevel=%z_NestLevel%-1
exit /b %z_ReturnCode%

:help
echo .
echo the FPARTS.BAT procedure is intended to return parts of a file's path as follows
echo z_fdrv  = drive letter
echo z_fpath = path to file
echo z_fname = file name
echo z_fext  = file extension
echo .
set z_ReturnCode=16
goto return
