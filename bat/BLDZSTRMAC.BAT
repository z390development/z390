@if /I %1. == tron. (echo on) else (echo off)
rem generate HLASM compatible versions of structured programming macros

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
set z_Dest=%1
if "%z_Dest%"=="" set z_Dest=build\zstrmac
echo %0 Destination: %z_Dest%
if not exist "%z_Dest%" mkdir %z_Dest%
echo Deleting %z_Dest%\*.*
erase /Q %z_Dest%\*.*

set z_STR=rt\test\ZSTRMAC2.ZSM

rem Generate the HLASM versions of the macros
set z_MACS=( CASE CASENTRY DO ELSE ELSEIF ENDCASE ENDDO ENDIF ENDSEL IF OTHRWISE SELECT WHEN ZSTREQU )
for %%M in %z_MACS% do (set SYSUT1=mac\%%M.MAC
                        set SYSUT2=%z_Dest%\%%M.MAC
                        echo SYSUT1=mac\%%M.MAC SYSUT2=%z_Dest%\%%M.MAC bat\MZ390 %z_TraceMode% %z_STR% noasm stats
                        call bat\MZ390 %z_TraceMode% %z_STR% noasm stats || goto error
                        )

rem Copy the static copy files
set z_CPYS=(mac\ASMMSP.CPY mac\ZSTRGBL.CPY mac\ZSTRMAC.CPY)
for %%C in %z_CPYS% do (echo Adding %%C to %z_Dest%
                        copy %%C %z_Dest% >nul 2>&1 || goto error
                        )
set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%