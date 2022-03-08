@if /I %1. == tron. (echo on) else (echo off)
rem create/update file z390.properties with z390 version from git repository

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
cd..

set z_Version="Uncontrolled"

git --man-path >NUL

set z_Git_Installed=%ERRORLEVEL%
if %z_Git_Installed% NEQ 0 (echo Git is not installed
                            goto final
                            ) 

for /F "tokens=* USEBACKQ" %%F in (`git rev-parse --is-inside-work-tree`) do (set z_Git_Repo=%%F)
if "%z_Git_Repo%" NEQ "true" (echo Not a git repository
                              goto final)
for /F "tokens=* USEBACKQ" %%I in (`git describe`) do set z_Version=%%I

:final
if %z_Version% EQU "Uncontrolled" (if /I "%1" NEQ "FORCE" (echo Version is uncontrolled - add FORCE parm to ignore
                                                           set z_ReturnCode=8
                                                           goto return
                                                   ) else (echo Version is uncontrolled - FORCE enabled
                                   )                       )
rem relver: use git to set z390.properties version details
echo z390 version "%z_Version%"
echo version=%z_Version% > z390.properties
set z_ReturnCode=%ERRORLEVEL%

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
