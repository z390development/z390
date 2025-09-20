@if /I "%1" == "tron" (echo on) else (echo off)
rem regression test z390 optables
rem this procedure is intended to be called by RUNOPTABLES.BAT

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

rem check parameters
set option=%1
set optval=%2
set suffix=%3

rem %1 = optable / machine
if %option%. EQU . goto error1
if /I %option% NEQ OPTABLE if /I %option% NEQ MACHINE goto error10
if /I %option% EQU OPTABLE set infix=#
if /I %option% EQU MACHINE set infix=_
rem
rem %2 = optable or machine option value
if %optval%. EQU . goto error2
if not exist rt\mlc\OPTB%infix%%optval%.OPT goto error20
rem
rem %3 = Suffix for reference file (defaults to option value when option is optable)
if %3. EQU . if /I %option% EQU OPTABLE set suffix=%2
if %suffix%. EQU . goto error3
if /I %suffix% NEQ *None if not exist rt\mlc\OPTB#%suffix%.TF1 goto error30
if /I %suffix% EQU *None (set asmbat=asml) else (set asmbat=asmlg)
rem
rem no more parameters expected
if %4. NEQ . goto error4

rem  %option% %optval%
set Z390OPT=rt\mlc\OPTB%infix%%optval%.OPT
set Z390PRN=rt\mlc\OPTB%infix%%optval%.PRN
set Z390ERR=rt\mlc\OPTB%infix%%optval%.ERR
set Z390STA=rt\mlc\OPTB%infix%%optval%.STA
set HLASMPRN=rt\mlc\OPTB#%suffix%.TF1
call bat\%asmbat%.bat %z_TraceMode% rt\mlc\OPTB# @%Z390OPT% sysprn(%Z390PRN%) syserr(%Z390ERR%) stats(%Z390STA%)
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 0 echo %0 - OPTB# "%option%(%optval%)" is okay
if %z_ReturnCode% NEQ 0 echo %0 ERROR: OPTB# returned %z_ReturnCode% for "%option%(%optval%) == OPTABLE(%suffix%)"
goto return

:error1
echo %0 ERROR: first argument missing. Specify OPTABLE or MACHINE.
goto return_err
:error10
echo %0 ERROR: first argument should be OPTABLE or MACHINE - %option% is not accepted.
goto return_err
:error2
echo %0 ERROR: second argument missing. Specify option value for %option%.
goto return_err
:error20
echo %0 ERROR: options file for %optval% rt\mlc\OPTB%infix%%optval%.opt was not found.
goto return_err
:error3
echo %0 ERROR: third argument missing. Specify option value for reference file.
goto return_err
:error30
echo %0 ERROR: reference file for %suffix% rt\mlc\OPTB#%suffix%.TF1 was not found.
goto return_err
:error4
echo %0 ERROR: unexpected argument(s): %4 %5 %6 %7 %8 %9
:return_err
set z_ReturnCode=12

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %option% %optval% %suffix% %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
