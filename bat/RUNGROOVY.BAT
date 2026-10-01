@if /I "%1" == "tron" (echo on) else (echo off)
rem rungroovy runs a groovy regression test suite, or a script (or part of it) using gradle

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
pushd %~dps0..\z390test

rem -- determine testmode: full includes optional tests
set testMode=standard
if /I "%1" == "full" (set testMode=full
                      shift /1
              )

rem -- run the test or tests
if %1. == . (call gradlew test --rerun -PtestMode=%testMode%
     ) else (call gradlew test --rerun -PtestMode=%testMode% --tests %1 %2 %3 %4 %5 %6 %7 %8 %9
             )
set z_ReturnCode=%ERRORLEVEL%

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
