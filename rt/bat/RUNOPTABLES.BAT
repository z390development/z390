@if /I "%1" == "tron" (echo on) else (echo off)
rem regression test z390 instructions

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
set z_MaxRetCode=0

rem  Optable DOS
set Z390PRN=rt\rt\OPCD$DOS.PRN
set HLASMPRN=rt\rt\OPCD$DOS.TF1
set SYSOUT=rt\rt\OPCD$DOS.OUT
call bat\asmlg.bat %z_TraceMode% rt\rt\OPCD$DOS @rt\rt\OPCD$DOS.OPT
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPCD$DOS "optable(DOS)" is okay
                         ) else (
                         echo %0 ERROR: OPCD$DOS returned %z_ReturnCode% for "optable(DOS)"
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_MaxRetCode%
