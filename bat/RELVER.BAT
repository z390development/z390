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
pushd %~dps0..                                       || goto nopush

if not exist version.txt (
    echo %0 ERROR: version.txt not found
    set z_ReturnCode=8
    goto return
)

set /p VERSION=<version.txt                          || goto error
echo z390 version "%VERSION%"
echo version=%VERSION%>z390.properties               || goto error
set z_ReturnCode=%ERRORLEVEL%
goto return

:nopush
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
goto exit
:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
:exit
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
