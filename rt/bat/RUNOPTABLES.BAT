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

set count_ok=0
set count_err=0
if /I %1. EQU /Q. goto Quick
if /I %1. NEQ .   goto %1
:Quick

rem *********************************************************************************************
rem
rem Section 1 optable option values
rem
rem *********************************************************************************************

rem  Optable 360-20
rem  this optable cannot be compared with HLASM - HLASM does not support this option
:360-20
set Z390PRN=rt\mlc\OPTB#320.PRN
call bat\asml.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#320.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(360-20)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(360-20)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable DOS
:DOS
set Z390PRN=rt\mlc\OPTB#DOS.PRN
set HLASMPRN=rt\mlc\OPTB#DOS.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#DOS.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(DOS)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(DOS)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable 370
:370
set Z390PRN=rt\mlc\OPTB#370.PRN
set HLASMPRN=rt\mlc\OPTB#370.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#370.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(370)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(370)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable XA
:XA
set Z390PRN=rt\mlc\OPTB#XA.PRN
set HLASMPRN=rt\mlc\OPTB#XA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#XA.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(XA)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(XA)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ESA
:ESA
set Z390PRN=rt\mlc\OPTB#ESA.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#ESA.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(ESA)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(ESA)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ZOP
:ZOP
set Z390PRN=rt\mlc\OPTB#ZOP.PRN
set HLASMPRN=rt\mlc\OPTB#ZOP.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#ZOP.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(ZOP)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(ZOP)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable uni
:uni
set Z390PRN=rt\mlc\OPTB#UNI.PRN
call bat\asml.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#UNI.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(uni)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(uni)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem -- end of optables. Quick run ends here
if %1. NEQ . goto report

rem *********************************************************************************************
rem
rem Section 2 machine option values
rem
rem *********************************************************************************************

rem  Machine S360-20 - equivalent of Optable(360-20)
rem  this optable cannot be compared with HLASM - HLASM does not support this option
:S360-20
set Z390PRN=rt\mlc\OPTB_S360-20.PRN
call bat\asml.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_S360-20.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(S360-20)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(S360-20)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine S370 - equivalent of Optable(370)
:S370
set Z390PRN=rt\mlc\OPTB_S370.PRN
set HLASMPRN=rt\mlc\OPTB#370.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_S370.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(S370)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(S370)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine S370XA - equivalent of Optable(XA)
:S370XA
set Z390PRN=rt\mlc\OPTB_S370XA.PRN
set HLASMPRN=rt\mlc\OPTB#XA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_S370XA.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(S370XA)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(S370XA)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine ARCH-0 - equivalent of Optable(XA)
:ARCH-0
set Z390PRN=rt\mlc\OPTB_ARCH-0.PRN
set HLASMPRN=rt\mlc\OPTB#XA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_ARCH-0.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(ARCH-0)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(ARCH-0)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine S370ESA - equivalent of Optable(ESA)
:S370ESA
set Z390PRN=rt\mlc\OPTB_S370ESA.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_S370ESA.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(S370ESA)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(S370ESA)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine S390 - equivalent of Optable(ESA)
:S390
set Z390PRN=rt\mlc\OPTB_S390.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_S390.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(S390)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(S390)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine S390E - equivalent of Optable(ESA)
:S390E
set Z390PRN=rt\mlc\OPTB_S390E.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_S390E.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(S390E)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(S390E)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine ARCH-1 - equivalent of Optable(ESA)
:ARCH-1
set Z390PRN=rt\mlc\OPTB_ARCH-1.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_ARCH-1.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(ARCH-1)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(ARCH-1)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine ARCH-2 - equivalent of Optable(ESA)
:ARCH-2
set Z390PRN=rt\mlc\OPTB_ARCH-2.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_ARCH-2.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(ARCH-2)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(ARCH-2)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine ARCH-3 - equivalent of Optable(ESA)
:ARCH-3
set Z390PRN=rt\mlc\OPTB_ARCH-3.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_ARCH-3.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(ARCH-3)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(ARCH-3)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine ARCH-4 - equivalent of Optable(ESA)
:ARCH-4
set Z390PRN=rt\mlc\OPTB_ARCH-4.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_ARCH-4.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(ARCH-4)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(ARCH-4)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

:report
echo %count_ok% tests were okay and %count_err% failed

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_MaxRetCode%
