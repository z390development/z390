@if /I "%1" == "tron" (echo on) else (echo off)
rem validate source code

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
pushd %~dps0..                        || goto nopush

rem the linter jar must already match the version pinned in src\checkstyle.xml
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0GetCheckStyle.ps1" -CheckVersionOnly
if %ERRORLEVEL% EQU 0 goto run_linter
if %ERRORLEVEL% EQU 2 goto need_linter
goto error

:run_linter
rem run the linter
java -jar build\checkstyle\checkstyle-all.jar --version                 || goto error
java -jar build\checkstyle\checkstyle-all.jar -c src\checkstyle.xml src || goto error
set z_ReturnCode=0
goto return

:need_linter
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: GetCheckStyle must be run to install or upgrade the linter
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
rem ----- Lvl(%z_NestLevel%) End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
