@if /I "%1" == "tron" (echo on) else (echo off)
rem mac run mz390 macro processor to expand mlc to bal with nomlc

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

if /I %1. == . (echo %0 ERROR: missing file name
                set z_ReturnCode=16
                goto return
                )

if exist %1.BAL erase %1.BAL
if exist %1.ERR erase %1.ERR
if exist %1.STA erase %1.STA
if exist %1.TR* erase %1.TR*
call %~dps0mz390 %z_TraceMode% %1 bal noasm sysmac(%~dps0mac+.) syscpy(%~dps0mac+.) chksrc(0) chkmac(0) %2 %3 %4 %5 %6 %7 %8 %9
set z_ReturnCode=%ERRORLEVEL%
rem any error or warning message has been issued by mz390
if %z_ReturnCode% NEQ 0 (if %z_ReturnCode% NEQ 4 (erase %1.OBJ
                                                  goto return))

:return
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
