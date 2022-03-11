@if /I %1. == tron. (echo on) else (echo off)
rem run vse demos under z390

setlocal
if /I %1. == tron. (set z_TraceMode=tron
                    shift /1
            ) else (if /I %1. == troff. (set z_TraceMode=troff
                                         shift /1
                                 ) else (set z_TraceMode=)
                    )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

pushd %~dp0\..
call bat\asmlg %z_TraceMode% vse\demo\DEMOVSE1 sysmac(vse\mac+mac) || goto error
call bat\asmlg %z_TraceMode% vse\demo\DEMOVSE2 sysmac(vse\mac+mac) || goto error
set SYSUT1=vse\demo\DEMOVSE3.TF1
set SYSUT2=vse\demo\DEMOVSE3.TF2
call bat\asmlg %z_TraceMode% vse\demo\DEMOVSE3 sysmac(vse\mac+mac) || goto error
comp /M vse\demo\DEMOVSE3.TF1 vse\demo\DEMOVSE3.TF2
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% GTR 0 (set z_ReturnCode=8
                         echo %0 ERROR: DEMOVSE3 data were not copied correctly
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
