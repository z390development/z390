@if /I %1. == tron. (echo on) else (echo off)
rem demo hello world demo

setlocal
if /I %1. == tron. (set z_TraceMode=tron
                    shift /1
            ) else (if /I %1. == troff. (set z_TraceMode=troff
                                         shift /1
                                 ) else (set z_TraceMode=)
                    )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

pushd %~dp0
if exist runasmdemos.bat cd ..

call bat\ASMLG %z_TraceMode% demo\HELLO    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASM   %z_TraceMode% demo\DEMOM8Q1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
set sysut1=demo\TESTDCB1.TF1
set sysut2=demo\TESTDCB1.TF2
set sysout=demo\TESTDCB1.TF3
call bat\ASMLG %z_TraceMode% demo\TESTDCB1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
comp /M demo\TESTDCB1.TF1 demo\TESTDCB1.TF2
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% GTR 0 (set z_ReturnCode=8
                         echo %0 ERROR: TESTDCB1 data were not copied correctly
                         goto return
                         )
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
