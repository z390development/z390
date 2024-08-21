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
call bat\bldjar         %z_TraceMode%          || goto error
call bat\bldlib         %z_TraceMode%          || goto error
call bat\bldzstrmac     %z_TraceMode%          || goto error
call bat\bldcbllib      %z_TraceMode%          || goto error

call z390test\gradlew.bat -p z390test test     || goto error

echo .
echo all RT scripts for z390 ended okay
set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
