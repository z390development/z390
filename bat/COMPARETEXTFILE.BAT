@if /I "%1" == "tron" (echo on) else (echo off)
rem Compare two text files. CRLF and LF are the same line-end.

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

if "%~1"=="" goto needtwo
if "%~2"=="" goto needtwo

powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0CompareTextFile.ps1" "%~1" "%~2"
set z_ReturnCode=%ERRORLEVEL%
goto exit

:needtwo
echo comparetextfile: two file paths are required 1>&2
set z_ReturnCode=2

:exit
rem ----- Lvl(%z_NestLevel%) End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
