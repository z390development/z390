@echo off
rem open z390 gui without keeping command line
setlocal
pushd %~dp0
cd..
start javaw -jar z390.jar
