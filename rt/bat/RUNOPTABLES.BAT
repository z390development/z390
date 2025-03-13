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

rem  Optable uni **!! comparison to be enabled when optable(uni) has been fixed **!!
:uni
call rt\bat\RUNOPTABLE %z_TraceMode% optable UNI *None
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 (set /A count_ok=%count_ok%+1) else (
                         set /A count_err=%count_err%+1
                         set err_optable=%err_optable% uni
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

:report
echo %count_ok% tests were okay and %count_err% failed. Returncode is %z_MaxRetCode%
IF %count_err% NEQ 0 echo Failed optables: %err_optable%
IF %count_err% NEQ 0 echo Failed machines: %err_machine%

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_MaxRetCode%
