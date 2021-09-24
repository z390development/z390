@echo off
setlocal
pushd %~dp0
cd ..

rem relver: use git to set z390.properties version details
FOR /F "delims=" %%i IN ('git describe') DO set version=%%i
echo version=%version% > z390.properties
