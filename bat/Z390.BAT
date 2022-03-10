@if /I %1. == tron. (echo on) else (echo off)
rem open z390 gui without keeping command line
setlocal
if /I %1. == tron. (set z_TraceMode=tron
                    shift /1
            ) else (if /I %1. == troff. (set z_TraceMode=troff
                                         shift /1
                                 ) else (set z_TraceMode=)
                    )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

pushd %~dp0
if exist z390.bat cd ..

start javaw -jar z390.jar

set z_ReturnCode=%ERRORLEVEL%
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
