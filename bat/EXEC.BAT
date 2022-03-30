@if /I "%1" == "tron" (echo on) else (echo off)
rem exec execute 390 load module

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

if /I %1. == . (set /P z_file=Hit Enter for help or suppply name of 390 file to execute: 
        ) else (set z_file=%1)
if /I %z_file%. == . goto help
:chkfile
rem convert Unix path notation to Windows before checking the file
set z_file=%z_file:/=\%
if not exist %z_file%.390 (echo %0 ERROR: %z_file%.390 was not found
                           set z_ReturnCode=16
                           goto return)
if exist %z_file%.LOG erase %z_file%.LOG
if exist %z_file%.ERR erase %z_file%.ERR
if exist %z_file%.STA erase %z_file%.STA
if exist %z_file%.TR* erase %z_file%.TR*
call %~dps0ez390 %z_TraceMode% %z_file% %2 %3 %4 %5 %6 %7 %8 %9
set z_ReturnCode=%ERRORLEVEL%
rem any error or warning message has been issued by ez390

:return
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%

:help
echo .
echo the EXEC.BAT procedure is intended to execute a single program
echo .
set /P z_file=Hit Enter for more help or suppply name of 390 file to execute: 
if /I %z_file%. NEQ . goto chkfile

call %~dps0help %z_TraceMode%
set z_ReturnCode=16
goto return
