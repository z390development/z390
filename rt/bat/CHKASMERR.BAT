@if /I "%1" == "tron" (echo on) else (echo off)
rem validate errors from failing assembly
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
echo %0 validate errors in %1
rem -- run the assembly, redirect output to avoid cluttering the log with the expected error messages
call bat\asm %z_TraceMode% %1 nocon notiming errsum >%1.tmp
rem -- compare error summary with the expected error summary output - which is saved in the .txt file
FC %1.ERR %1.TXT                                   >>%1.tmp
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 OKAY for %1 - actual output matches expectations
                 ) else (echo %0 ERROR on %1 - actual output does not match expectations
                         echo %0 ERROR on %1 - to analyze please compare %1.ERR with %1.TXT
                         set z_ReturnCode=8
                         goto return
                         )
del /Q %1.tmp
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
echo %1.TXT
type %1.TXT
echo %1.ERR
type %1.ERR
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
