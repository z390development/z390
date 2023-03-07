@if /I "%1" == "tron" (echo on) else (echo off)
rem create/update file z390.properties with z390 version from git repository

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
pushd %~dps0..
set z_Version="Uncontrolled"

rem -- is git installed?
git --version >NUL
set z_Git_Installed=%ERRORLEVEL%
if %z_Git_Installed% NEQ 0 (echo Git is not installed
                            goto final
                            )

rem -- are we in a git repository?
git rev-parse --is-inside-work-tree >NUL
set z_Git_WorkTree=%ERRORLEVEL%
if %z_Git_WorkTree% NEQ 0 (echo Git cannot determine whether we're in a work tree
                           goto final
                           )
for /F "tokens=* USEBACKQ" %%F in (`git rev-parse --is-inside-work-tree`) do (set z_Git_Repo=%%F)
if "%z_Git_Repo%" NEQ "true" (echo Not a git repository
                              goto final)

rem -- get git to describe the repo
git describe >NUL
set z_Git_Describe=%ERRORLEVEL%
if %z_Git_Describe% EQU 0 goto describe
echo Unable to git describe - fetching tags from upstream

rem -- make sure we have an upstream defined
git config remote.upstream.url >NUL
set z_Git_Remote=%ERRORLEVEL%
if %z_Git_Remote% EQU 0 goto fetch_upstream
rem -- use default to set the remote location
git remote add upstream https://github.com/z390development/z390.git
set z_Git_SetRemote=%ERRORLEVEL%
if %z_Git_SetRemote% NEQ 0 (echo Git cannot set remote
                            goto final
                            )

:fetch_upstream
rem -- fetch upstream to fix missing git tags
git fetch -p upstream
set z_Git_Fetch=%ERRORLEVEL%
if %z_Git_Fetch% NEQ 0 (echo Git cannot fetch from upstream
                        goto final
                        )

:describe
for /F "tokens=* USEBACKQ" %%I in (`git describe`) do set z_Version=%%I

:final
if %z_Version% EQU "Uncontrolled" (if /I "%1" NEQ "FORCE" (echo Version is uncontrolled - add FORCE parm to ignore
                                                           set z_ReturnCode=8
                                                           goto return
                                                   ) else (echo Version is uncontrolled - FORCE enabled
                                   )                       )
rem relver: use git to set z390.properties version details
echo z390 version "%z_Version%"
echo version=%z_Version%> z390.properties
set z_ReturnCode=%ERRORLEVEL%

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
