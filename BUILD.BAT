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

rem -- parse all parameters
set buildall=
set nist=
set runnist=
set arc=
:parmloop
if %1. == . goto parmsdone
if /I %1 == *All goto parm_all
if /I %1 == nist goto parm_nist
if /I %1 == arc  goto parm_arc
echo Error: unrecognized parameter: %1
echo Use *All include optional components in the test
echo     arc  <path to z390Archive> to run zvsam2 bulk load test
echo     nist <path to nistcobol85> to run zCobol against nist test suite
goto error

:parm_all
set buildall=%1
shift /1
goto parmloop
:parm_nist
set "nist=%~2"
set runnist=yes
shift /1
shift /1
goto parmloop
:parm_arc
set "arc=%~2"
shift /1
shift /1
goto parmloop
:parmsdone

rem Output Java version for CI/CD
java -version 2>&1 | echo ::set-output name=javaversion::%

rem build the package -- %1 might be *ALL to include non-standard components in the build
call bat\BLDDIST.BAT %z_TraceMode% %buildall% || goto error
call bat\IVP.BAT     %z_TraceMode% %buildall% || goto error

rem run the tests
call z390test\gradlew.bat -p z390test cleanTest || goto error
call z390test\gradlew.bat -p z390test test      || goto error

rem if arc was specified, we run the vsam2 tests
if /I "%arc%"  NEQ "" call vsam2\bat\TSTLOADIN.BAT %z_TraceMode% "%arc%" || goto error
rem if NIST was specified, run the NIST test suite (will error always)
if /I "%nist%" NEQ "" call bat\RUNNIST.BAT %z_TraceMode% "%nist%"

set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%) End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
