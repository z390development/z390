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
set optb=320
set opt=360-20
set Z390PRN=rt\mlc\OPTB#%optb%.PRN
call bat\asml.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#%optb%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(%opt%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(%opt%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable DOS
:DOS
set optb=DOS
set Z390PRN=rt\mlc\OPTB#%optb%.PRN
set HLASMPRN=rt\mlc\OPTB#%optb%.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#%optb%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(%optb%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(%optb%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable 370
:370
set optb=370
set Z390PRN=rt\mlc\OPTB#%optb%.PRN
set HLASMPRN=rt\mlc\OPTB#%optb%.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#%optb%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(%optb%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(%optb%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable XA
:XA
set optb=XA
set Z390PRN=rt\mlc\OPTB#%optb%.PRN
set HLASMPRN=rt\mlc\OPTB#%optb%.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#%optb%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(%optb%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(%optb%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ESA
:ESA
set optb=ESA
set Z390PRN=rt\mlc\OPTB#%optb%.PRN
set HLASMPRN=rt\mlc\OPTB#%optb%.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#%optb%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(%optb%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(%optb%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ZOP - Synonym of ZS1
:ZOP
if %1. NEQ . if /I %1. EQU /Q. goto ZS1
set optb=ZOP
set Z390PRN=rt\mlc\OPTB#%optb%.PRN
set HLASMPRN=rt\mlc\OPTB#%optb%.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#%optb%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(%optb%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(%optb%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ZS1 - Synonym of ZOP
:ZS1
set optb=ZS1
set Z390PRN=rt\mlc\OPTB#%optb%.PRN
set HLASMPRN=rt\mlc\OPTB#%optb%.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#%optb%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(%optb%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(%optb%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable uni
:uni
set optb=UNI
set Z390PRN=rt\mlc\OPTB#%optb%.PRN
call bat\asml.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB#%optb%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "optable(%optb%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "optable(%optb%)"
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
set mac=S360-20
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
call bat\asml.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine S370 - equivalent of Optable(370)
:S370
set mac=S370
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#370.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine S370XA - equivalent of Optable(XA)
:S370XA
set mac=S370XA
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#XA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine ARCH-0 - equivalent of Optable(XA)
:ARCH-0
set mac=ARCH-0
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#XA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine S370ESA - equivalent of Optable(ESA)
:S370ESA
set mac=S370ESA
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine S390 - equivalent of Optable(ESA)
:S390
set mac=S390
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine S390E - equivalent of Optable(ESA)
:S390E
set mac=S390E
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine ARCH-1 - equivalent of Optable(ESA)
:ARCH-1
set mac=ARCH-1
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine ARCH-2 - equivalent of Optable(ESA)
:ARCH-2
set mac=ARCH-2
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine ARCH-3 - equivalent of Optable(ESA)
:ARCH-3
set mac=ARCH-3
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine ARCH-4 - equivalent of Optable(ESA)
:ARCH-4
set mac=ARCH-4
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#ESA.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine zSeries - equivalent of Optable(ZS1)
:zSeries
set mac=zSeries
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#ZS1.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine zSeries-1 - equivalent of Optable(ZS1)
:zSeries-1
set mac=zSeries-1
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#ZS1.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine ZS - equivalent of Optable(ZS1)
:ZS
set mac=ZS
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#ZS1.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine ZS-1 - equivalent of Optable(ZS1)
:ZS-1
set mac=ZS-1
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#ZS1.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine z800 - equivalent of Optable(ZS1)
:z800
set mac=z800
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#ZS1.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine z900 - equivalent of Optable(ZS1)
:z900
set mac=z900
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#ZS1.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
                         set /A count_err=%count_err%+1
                         if (%z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%)
                         )
if %1. NEQ . goto return

rem  Machine ARCH-5 - equivalent of Optable(ZS1)
:ARCH-5
set mac=ARCH-5
set Z390PRN=rt\mlc\OPTB_%mac%.PRN
set HLASMPRN=rt\mlc\OPTB#ZS1.TF1
call bat\asmlg.bat %z_TraceMode% rt\mlc\OPTB# @rt\mlc\OPTB_%mac%.OPT sysprn(%Z390PRN%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (echo %0 - OPTB# "machine(%mac%)" is okay
                         set /A count_ok=%count_ok%+1
                         ) else (
                         echo %0 ERROR: OPTB# returned %z_ReturnCode% for "machine(%mac%)"
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
