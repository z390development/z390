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
call bat\bldcbllib      %z_TraceMode%          || goto error
call rt\bat\zopcheck    %z_TraceMode%          || goto error
call bat\runasmdemos    %z_TraceMode%          || goto error
call bat\runcbldemos    %z_TraceMode%          || goto error
call rt\bat\runasmtests %z_TraceMode%          || goto error
call rt\bat\runcbltests %z_TraceMode%          || goto error
call rt\bat\runrtqsam   %z_TraceMode% TIME(30) || goto error
call rt\bat\runrtbsam   %z_TraceMode%          || goto error
call rt\bat\runsort     %z_TraceMode%          || goto error
call rt\bat\runzpar     %z_TraceMode%          || goto error
call bat\runassist      %z_TraceMode%          || goto error
call bat\runcmdproc     %z_TraceMode%          || goto error
call bat\runvsedemos    %z_TraceMode%          || goto error
call bat\runzstrmactest %z_TraceMode%          || goto error
call rt\bat\runtbrctx   %z_TraceMode%          || goto error
call rt\bat\runtestopt  %z_TraceMode%          || goto error
call rt\bat\runbr14owe  %z_TraceMode%          || goto error
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
