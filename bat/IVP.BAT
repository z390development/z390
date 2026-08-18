@if /I "%1" == "tron" (echo on) else (echo off)
rem execute mz390 runtime with java runtime options:

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

java -classpath %~dps0..\z390.jar -Xrs -Xms150000K -Xmx150000K %J2SEOPTIONS% iz390 %1 %2 %3 %4 %5 %6 %7 %8 %9

set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0  goto return
if %z_ReturnCode% EQU 4 (echo %0 WARNING: See warnings on iz390 generated output
                         goto return
                         )
echo %0 ERROR: See errors on iz390 generated output

:return
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
