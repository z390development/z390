@if /I "%1" == "tron" (echo on) else (echo off)
rem rebuild z390 operational environment from source

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
pushd %~dps0

rem Output Java version for CI/CD
java -version 2>&1 | echo ::set-output name=javaversion::%

rem build the package -- %1 might be *ALL to include non-standard components in the build
call bat\BLDDIST.BAT %z_TraceMode% %*           || goto error

rem run the tests
call z390test\gradlew.bat -p z390test cleanTest || goto error
call z390test\gradlew.bat -p z390test test      || goto error

rem if NIST was specified, it must be followed by the NIST repository's path
if /I %1. == nist. call bat\RUNNIST.BAT %z_TraceMode% %2
if /I %2. == nist. call bat\RUNNIST.BAT %z_TraceMode% %3
if /I %3. == nist. call bat\RUNNIST.BAT %z_TraceMode% %4

set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%) End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
