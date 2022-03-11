@echo off
setlocal
pushd %~dp0
cd ..
cd ..
REM RUN ZPAR ASM AND CBL DEMOS
CALL BAT\ASMLG DEMO\HELLO TRACE || goto error
CALL BAT\MAC ZPAR\ZPARTRS SYSPARM(DEMO\HELLO) || goto error
CALL BAT\CBLCLG ZCOBOL\DEMO\HELLO TRACE || goto error
CALL BAT\MAC ZPAR\ZPARTRS SYSPARM(ZCOBOL\DEMO\HELLO) || goto error
@REM PAUSE SEE GENERATED TRS FILES TO VERIFY ZPAR DEMOS OK

goto:eof

:error
echo.RUNZPAR  ERROR: Encountered RC %ERRORLEVEL% - exiting
exit /b %ERRORLEVEL%

rem ======================================================================================
:new

@if /I "%1" == "tron" (echo on) else (echo off)
rem RUN ZPAR ASM AND CBL DEMOS
setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

pushd %~dp0\..\..
call bat\ASMLG  %z_TraceMode% demo\HELLO                                   || goto error
call bat\MAC    %z_TraceMode% zpar\ZPARTRS      SYSPARM(demo\HELLO)        || goto error
call bat\CBLCLG %z_TraceMode% zcobol\demo\HELLO TRACE                      || goto error
call bat\MAC    %z_TraceMode% zpar\ZPARTRS      SYSPARM(zcobol\demo\HELLO) || goto error
@REM PAUSE SEE GENERATED TRS FILES TO VERIFY ZPAR DEMOS OK
set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
