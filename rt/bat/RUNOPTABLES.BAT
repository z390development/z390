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
set err_optable=
set err_machine=
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
call rt\bat\RUNOPTABLE %z_TraceMode% optable 360-20 *None
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% 360-20
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable DOS
:DOS
call rt\bat\RUNOPTABLE %z_TraceMode% optable DOS
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% DOS
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable 370
:370
call rt\bat\RUNOPTABLE %z_TraceMode% optable 370
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% 370
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable XA
:XA
call rt\bat\RUNOPTABLE %z_TraceMode% optable XA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% XA
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ESA
:ESA
call rt\bat\RUNOPTABLE %z_TraceMode% optable ESA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% ESA
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ZOP - Synonym of ZS1
:ZOP
if %1. NEQ . if /I %1. EQU /Q. goto ZS1
call rt\bat\RUNOPTABLE %z_TraceMode% optable ZOP
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% ZOP
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ZS1 - Synonym of ZOP
:ZS1
call rt\bat\RUNOPTABLE %z_TraceMode% optable ZS1
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% ZS1
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable YOP - Synonym of ZS2
:YOP
if %1. NEQ . if /I %1. EQU /Q. goto ZS2
call rt\bat\RUNOPTABLE %z_TraceMode% optable YOP
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% YOP
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ZS2 - Synonym of YOP
:ZS2
call rt\bat\RUNOPTABLE %z_TraceMode% optable ZS2
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% ZS2
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable Z9 - Synonym of ZS3
:Z9
if %1. NEQ . if /I %1. EQU /Q. goto ZS3
call rt\bat\RUNOPTABLE %z_TraceMode% optable Z9
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% Z9
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ZS3 - Synonym of Z9
:ZS3
call rt\bat\RUNOPTABLE %z_TraceMode% optable ZS3
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% ZS3
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable Z10 - Synonym of ZS4
:Z10
if %1. NEQ . if /I %1. EQU /Q. goto ZS4
call rt\bat\RUNOPTABLE %z_TraceMode% optable Z10
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% Z10
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ZS4 - Synonym of Z10
:ZS4
call rt\bat\RUNOPTABLE %z_TraceMode% optable ZS4
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% ZS4
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable Z11 - Synonym of ZS5
:Z11
if %1. NEQ . if /I %1. EQU /Q. goto ZS5
call rt\bat\RUNOPTABLE %z_TraceMode% optable Z11
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% Z11
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ZS5 - Synonym of Z11
:ZS5
call rt\bat\RUNOPTABLE %z_TraceMode% optable ZS5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% ZS5
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable Z12 - Synonym of ZS6
:Z12
if %1. NEQ . if /I %1. EQU /Q. goto ZS6
call rt\bat\RUNOPTABLE %z_TraceMode% optable Z12
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% Z12
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ZS6 - Synonym of Z12
:ZS6
call rt\bat\RUNOPTABLE %z_TraceMode% optable ZS6
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% ZS6
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable Z13 - Synonym of ZS7
:Z13
if %1. NEQ . if /I %1. EQU /Q. goto ZS7
call rt\bat\RUNOPTABLE %z_TraceMode% optable Z13
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% Z13
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ZS7 - Synonym of Z13
:ZS7
call rt\bat\RUNOPTABLE %z_TraceMode% optable ZS7
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% ZS7
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable Z14 - Synonym of ZS8
:Z14
if %1. NEQ . if /I %1. EQU /Q. goto ZS8
call rt\bat\RUNOPTABLE %z_TraceMode% optable Z14
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% Z14
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ZS8 - Synonym of Z14
:ZS8
call rt\bat\RUNOPTABLE %z_TraceMode% optable ZS8
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% ZS8
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable Z15 - Synonym of ZS9
:Z15
if %1. NEQ . if /I %1. EQU /Q. goto ZS9
call rt\bat\RUNOPTABLE %z_TraceMode% optable Z15
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% Z15
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ZS9 - Synonym of Z15
:ZS9
call rt\bat\RUNOPTABLE %z_TraceMode% optable ZS9
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% ZS9
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable Z16 - Synonym of ZSA
:Z16
if %1. NEQ . if /I %1. EQU /Q. goto ZSA
call rt\bat\RUNOPTABLE %z_TraceMode% optable Z16
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% Z16
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable ZSA - Synonym of Z16
:ZSA
call rt\bat\RUNOPTABLE %z_TraceMode% optable ZSA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% ZSA
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . if /I %1. NEQ /Q. goto return

rem  Optable UNI
:UNI
call rt\bat\RUNOPTABLE %z_TraceMode% optable UNI
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% UNI
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
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
call rt\bat\RUNOPTABLE %z_TraceMode% machine S360-20 *None
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% S360-20
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine S370 - equivalent of Optable(370)
:S370
call rt\bat\RUNOPTABLE %z_TraceMode% machine S370 370
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% S370
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine S370XA - equivalent of Optable(XA)
:S370XA
call rt\bat\RUNOPTABLE %z_TraceMode% machine S370XA XA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% S370XA
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ARCH-0 - equivalent of Optable(XA)
:ARCH-0
call rt\bat\RUNOPTABLE %z_TraceMode% machine ARCH-0 XA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ARCH-0
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine S370ESA - equivalent of Optable(ESA)
:S370ESA
call rt\bat\RUNOPTABLE %z_TraceMode% machine S370ESA ESA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% S370ESA
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine S390 - equivalent of Optable(ESA)
:S390
call rt\bat\RUNOPTABLE %z_TraceMode% machine S390 ESA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% S390
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine S390E - equivalent of Optable(ESA)
:S390E
call rt\bat\RUNOPTABLE %z_TraceMode% machine S390E ESA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% S390E
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ARCH-1 - equivalent of Optable(ESA)
:ARCH-1
call rt\bat\RUNOPTABLE %z_TraceMode% machine ARCH-1 ESA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ARCH-1
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ARCH-2 - equivalent of Optable(ESA)
:ARCH-2
call rt\bat\RUNOPTABLE %z_TraceMode% machine ARCH-2 ESA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ARCH-2
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ARCH-3 - equivalent of Optable(ESA)
:ARCH-3
call rt\bat\RUNOPTABLE %z_TraceMode% machine ARCH-3 ESA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ARCH-3
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ARCH-4 - equivalent of Optable(ESA)
:ARCH-4
call rt\bat\RUNOPTABLE %z_TraceMode% machine ARCH-4 ESA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ARCH-4
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine zSeries - equivalent of Optable(ZS1)
:zSeries
call rt\bat\RUNOPTABLE %z_TraceMode% machine zSeries ZS1
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% zSeries
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine zSeries-1 - equivalent of Optable(ZS1)
:zSeries-1
call rt\bat\RUNOPTABLE %z_TraceMode% machine zSeries-1 ZS1
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% zSeries-1
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ZS - equivalent of Optable(ZS1)
:ZS
call rt\bat\RUNOPTABLE %z_TraceMode% machine ZS ZS1
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ZS
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ZS-1 - equivalent of Optable(ZS1)
:ZS-1
call rt\bat\RUNOPTABLE %z_TraceMode% machine ZS-1 ZS1
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ZS-1
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine z800 - equivalent of Optable(ZS1)
:z800
call rt\bat\RUNOPTABLE %z_TraceMode% machine z800 ZS1
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% z800
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine z900 - equivalent of Optable(ZS1)
:z900
call rt\bat\RUNOPTABLE %z_TraceMode% machine z900 ZS1
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% z900
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ARCH-5 - equivalent of Optable(ZS1)
:ARCH-5
call rt\bat\RUNOPTABLE %z_TraceMode% machine ARCH-5 ZS1
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ARCH-5
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine z890 - equivalent of Optable(ZS2)
:z890
call rt\bat\RUNOPTABLE %z_TraceMode% machine z890 ZS2
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% z890
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine z990 - equivalent of Optable(ZS2)
:z990
call rt\bat\RUNOPTABLE %z_TraceMode% machine z990 ZS2
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% z990
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine zSeries-2 - equivalent of Optable(ZS2)
:zSeries-2
call rt\bat\RUNOPTABLE %z_TraceMode% machine zSeries-2 ZS2
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% zSeries-2
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ZS-2 - equivalent of Optable(ZS2)
:ZS-2
call rt\bat\RUNOPTABLE %z_TraceMode% machine ZS-2 ZS2
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ZS-2
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ARCH-6 - equivalent of Optable(ZS2)
:ARCH-6
call rt\bat\RUNOPTABLE %z_TraceMode% machine ARCH-6 ZS2
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ARCH-6
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine z9 - equivalent of Optable(ZS3)
:machine_z9
call rt\bat\RUNOPTABLE %z_TraceMode% machine z9 ZS3
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% z9
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine zSeries-3 - equivalent of Optable(ZS3)
:zSeries-3
call rt\bat\RUNOPTABLE %z_TraceMode% machine zSeries-3 ZS3
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% zSeries-3
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ZS-3 - equivalent of Optable(ZS3)
:ZS-3
call rt\bat\RUNOPTABLE %z_TraceMode% machine ZS-3 ZS3
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ZS-3
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ARCH-7 - equivalent of Optable(ZS3)
:ARCH-7
call rt\bat\RUNOPTABLE %z_TraceMode% machine ARCH-7 ZS3
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ARCH-7
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine z10 - equivalent of Optable(ZS4)
:machine_z10
call rt\bat\RUNOPTABLE %z_TraceMode% machine z10 ZS4
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% z10
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine zSeries-4 - equivalent of Optable(ZS4)
:zSeries-4
call rt\bat\RUNOPTABLE %z_TraceMode% machine zSeries-4 ZS4
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% zSeries-4
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ZS-4 - equivalent of Optable(ZS4)
:ZS-4
call rt\bat\RUNOPTABLE %z_TraceMode% machine ZS-4 ZS4
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ZS-4
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ARCH-8 - equivalent of Optable(ZS4)
:ARCH-8
call rt\bat\RUNOPTABLE %z_TraceMode% machine ARCH-8 ZS4
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ARCH-8
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine z11 - equivalent of Optable(ZS5)
:machine_z11
call rt\bat\RUNOPTABLE %z_TraceMode% machine z11 ZS5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% z11
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine z114 - equivalent of Optable(ZS5)
:machine_z114
call rt\bat\RUNOPTABLE %z_TraceMode% machine z114 ZS5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% z114
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine z196 - equivalent of Optable(ZS5)
:machine_z196
call rt\bat\RUNOPTABLE %z_TraceMode% machine z196 ZS5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% z196
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine zSeries-5 - equivalent of Optable(ZS5)
:zSeries-5
call rt\bat\RUNOPTABLE %z_TraceMode% machine zSeries-5 ZS5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% zSeries-5
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ZS-5 - equivalent of Optable(ZS5)
:ZS-5
call rt\bat\RUNOPTABLE %z_TraceMode% machine ZS-5 ZS5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ZS-5
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ARCH-9 - equivalent of Optable(ZS5)
:ARCH-9
call rt\bat\RUNOPTABLE %z_TraceMode% machine ARCH-9 ZS5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ARCH-9
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine z12 - equivalent of Optable(ZS6)
:machine_z12
call rt\bat\RUNOPTABLE %z_TraceMode% machine z12 ZS6
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% z12
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine zBC12 - equivalent of Optable(ZS6)
:machine_zBC12
call rt\bat\RUNOPTABLE %z_TraceMode% machine zBC12 ZS6
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% zBC12
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine zEC12 - equivalent of Optable(ZS6)
:machine_zEC12
call rt\bat\RUNOPTABLE %z_TraceMode% machine zEC12 ZS6
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% zEC12
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine zSeries-6 - equivalent of Optable(ZS6)
:zSeries-6
call rt\bat\RUNOPTABLE %z_TraceMode% machine zSeries-6 ZS6
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% zSeries-6
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ZS-6 - equivalent of Optable(ZS6)
:ZS-6
call rt\bat\RUNOPTABLE %z_TraceMode% machine ZS-6 ZS6
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ZS-6
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ARCH-10 - equivalent of Optable(ZS6)
:ARCH-10
call rt\bat\RUNOPTABLE %z_TraceMode% machine ARCH-10 ZS6
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ARCH-10
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine z13 - equivalent of Optable(ZS7)
:machine_z13
call rt\bat\RUNOPTABLE %z_TraceMode% machine z13 ZS7
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% z13
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine zSeries-7 - equivalent of Optable(ZS7)
:zSeries-7
call rt\bat\RUNOPTABLE %z_TraceMode% machine zSeries-7 ZS7
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% zSeries-7
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ZS-7 - equivalent of Optable(ZS7)
:ZS-7
call rt\bat\RUNOPTABLE %z_TraceMode% machine ZS-7 ZS7
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ZS-7
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ARCH-11 - equivalent of Optable(ZS7)
:ARCH-11
call rt\bat\RUNOPTABLE %z_TraceMode% machine ARCH-11 ZS7
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ARCH-11
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine z14 - equivalent of Optable(ZS8)
:machine_z14
call rt\bat\RUNOPTABLE %z_TraceMode% machine z14 ZS8
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% z14
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine zSeries-8 - equivalent of Optable(ZS8)
:zSeries-8
call rt\bat\RUNOPTABLE %z_TraceMode% machine zSeries-8 ZS8
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% zSeries-8
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ZS-8 - equivalent of Optable(ZS8)
:ZS-8
call rt\bat\RUNOPTABLE %z_TraceMode% machine ZS-8 ZS8
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ZS-8
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ARCH-12 - equivalent of Optable(ZS8)
:ARCH-12
call rt\bat\RUNOPTABLE %z_TraceMode% machine ARCH-12 ZS8
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ARCH-12
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine z15 - equivalent of Optable(ZS9)
:machine_z15
call rt\bat\RUNOPTABLE %z_TraceMode% machine z15 ZS9
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% z15
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine zSeries-9 - equivalent of Optable(ZS9)
:zSeries-9
call rt\bat\RUNOPTABLE %z_TraceMode% machine zSeries-9 ZS9
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% zSeries-9
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ZS-9 - equivalent of Optable(ZS9)
:ZS-9
call rt\bat\RUNOPTABLE %z_TraceMode% machine ZS-9 ZS9
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ZS-9
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ARCH-13 - equivalent of Optable(ZS9)
:ARCH-13
call rt\bat\RUNOPTABLE %z_TraceMode% machine ARCH-13 ZS9
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ARCH-13
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine z16 - equivalent of Optable(ZSA)
:machine_z16
call rt\bat\RUNOPTABLE %z_TraceMode% machine z16 ZSA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% z16
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine zSeries-10 - equivalent of Optable(ZSA)
:zSeries-10
call rt\bat\RUNOPTABLE %z_TraceMode% machine zSeries-10 ZSA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% zSeries-10
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ZS-10 - equivalent of Optable(ZSA)
:ZS-10
call rt\bat\RUNOPTABLE %z_TraceMode% machine ZS-10 ZSA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ZS-10
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

rem  Machine ARCH-14 - equivalent of Optable(ZSA)
:ARCH-14
call rt\bat\RUNOPTABLE %z_TraceMode% machine ARCH-14 ZSA
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_machine=%err_machine% ARCH-14
                         if %z_ReturnCode% GTR %z_MaxRetCode% (set z_MaxRetCode=%z_ReturnCode%) )
if %1. NEQ . goto return

:report
echo %count_ok% tests were okay and %count_err% failed. Returncode is %z_MaxRetCode%
IF %count_err% NEQ 0 echo Failed optables: %err_optable%
IF %count_err% NEQ 0 echo Failed machines: %err_machine%

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_MaxRetCode%