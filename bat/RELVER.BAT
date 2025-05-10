@if /I "%1" == "tron" (echo on) else (echo off)
rem Get z390 version from version.txt file and create z390.properties

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

if not exist version.txt (
    echo %0 ERROR: version.txt not found
    set z_ReturnCode=8
    goto return
)

set /p VERSION=<version.txt
echo z390 version "%VERSION%"
echo version=%VERSION%>z390.properties
set z_ReturnCode=%ERRORLEVEL%

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
