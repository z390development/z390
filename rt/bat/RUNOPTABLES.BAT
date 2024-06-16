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
call bat\asmlg.bat %z_TraceMode% rt\rt\OPCD$ @rt\rt\OPCD$DOS.OPT sysprn(rt\rt\OPCD$DOS.PRN)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPCD$ "optable(DOS)" is okay
                         ) else (
                         echo %0 ERROR: OPCD$ returned %z_ReturnCode% for "optable(DOS)"
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )

rem  Optable 370
set Z390PRN=rt\rt\OPCD$370.PRN
set HLASMPRN=rt\rt\OPCD$370.TF1
call bat\asmlg.bat %z_TraceMode% rt\rt\OPCD$ @rt\rt\OPCD$370.OPT sysprn(rt\rt\OPCD$370.PRN)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPCD$ "optable(370)" is okay
                         ) else (
                         echo %0 ERROR: OPCD$ returned %z_ReturnCode% for "optable(370)"
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_MaxRetCode%
