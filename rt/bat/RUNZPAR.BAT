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

pushd %~dps0..\..
rem -- sorry TRON mode not fully supported
CALL BAT\ASMLG  %z_TraceMode% DEMO\HELLO        TRACE                      || goto error
CALL BAT\MAC    troff         ZPAR\ZPARTRS      SYSPARM(DEMO\HELLO)        || goto error
CALL BAT\CBLCLG %z_TraceMode% ZCOBOL\DEMO\HELLO TRACE                      || goto error
CALL BAT\MAC    troff         ZPAR\ZPARTRS      SYSPARM(ZCOBOL\DEMO\HELLO) || goto error
REM PAUSE SEE GENERATED TRS FILES TO VERIFY ZPAR DEMOS OK

rem -- resume tron mode if relevant
if /I "%z_TraceMode%" == "tron" echo on
rem -- tron mode now activated again

set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
