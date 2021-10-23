@echo off
setlocal
pushd %~dp0
cd ..

SET VER="Uncontrolled"

git --man-path >NUL

SET GIT_INSTALLED=%ERRORLEVEL%
if %GIT_INSTALLED% NEQ 0 (
    echo Git is not installed
    goto :final
) 

FOR /F "tokens=* USEBACKQ" %%F IN (`git rev-parse --is-inside-work-tree`) DO (SET GIT_REPO=%%F)

if "%GIT_REPO%" NEQ "true" (
    echo Not a git repo
    goto :final
) ELSE (
    FOR /F "tokens=* USEBACKQ" %%I IN (`git describe`) DO set VER=%%I
)

:final
if %VER% EQU "Uncontrolled" (
    if /I "%1" NEQ "FORCE" (
        echo Version is uncontrolled - add FORCE parm to ignore
        exit /b 8
    ) ELSE (
        echo Version is Uncontrolled - FORCE enabled
    )
)
rem relver: use git to set z390.properties version details
echo version=%VER% > z390.properties
