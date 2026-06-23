@if /I "%1" == "tron" (echo on) else (echo off)
rem run NIST cobol test suite

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
if %1. == . goto notNist
if not exist %1\z390\RUNNIST.BAT goto notNist
rem -- call NIST repo's RUNNIST.bat using our local full path name
for %%I in ("%~dp0..") do call %1\z390\RUNNIST.BAT "%%~fI" || goto error
goto return

:notNist
set z_ReturnCode=16
echo %0 ERROR: "%1" is not a path to a clone of z390development/nistcobol85
goto return
:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
goto return
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
