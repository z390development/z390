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

rem  Optable ESA
set Z390PRN=rt\mlc\OPTB#ESA.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#ESA.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(ESA)" is okay
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(ESA)"
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
rem goto return

rem  Optable 360-20 (cannot compare with HLASM - HLASM does not support this option)
set Z390PRN=rt\mlc\OPTB#320.PRN
call bat\asml.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#320.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(360-20)" is okay
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(360-20)"
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )

rem  Machine S360-20 (cannot compare with HLASM - HLASM does not support this option)
set Z390PRN=rt\mlc\OPTB_S360-20.PRN
call bat\asml.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_S360-20.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(S360-20)" is okay
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(S360-20)"
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )

rem  Optable DOS
set Z390PRN=rt\mlc\OPTB#DOS.PRN
set HLASMPRN=rt\mlc\OPTB#DOS.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#DOS.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(DOS)" is okay
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(DOS)"
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )

rem  Optable 370
set Z390PRN=rt\mlc\OPTB#370.PRN
set HLASMPRN=rt\mlc\OPTB#370.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#370.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(370)" is okay
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(370)"
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )

rem  Machine S370
set Z390PRN=rt\mlc\OPTB_S370.PRN
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_S370.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(S370)" is okay
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(S370)"
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )

rem  Optable XA
set Z390PRN=rt\mlc\OPTB#XA.PRN
set HLASMPRN=rt\mlc\OPTB#XA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#XA.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(XA)" is okay
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(XA)"
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )

rem  Machine S370XA
set Z390PRN=rt\mlc\OPTB_S370XA.PRN
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_S370XA.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(S370XA)" is okay
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(S370XA)"
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )

rem  Machine ARCH-0
set Z390PRN=rt\mlc\OPTB_ARCH-0.PRN
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_ARCH-0.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(ARCH-0)" is okay
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(ARCH-0)"
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_MaxRetCode%
