@echo off
setlocal
pushd %~dp0
cd..
cd..
rem regression test z390 instructions
call bat\asm   tests\testins1 trace sysmac(mac) || goto error
call bat\asmlg tests\testins2 trace sysmac(mac) || goto error
call bat\asmlg tests\testins3 trace sysmac(mac) || goto error
call bat\asmlg tests\testins4 trace sysmac(mac) || goto error
call bat\asmlg tests\testins5 trace sysmac(mac) optable(z390) || goto error
call bat\asmlg tests\testdfp1 trace sysmac(mac) optable(z390) || goto error
call bat\asmlg tests\testdfp2 trace sysmac(mac) optable(z390) || goto error
@REM pause verify tests ran without errors

goto:eof

:error
echo RUNASMTE.ERROR: Encountered RC %ERRORLEVEL% - exiting
exit /b %ERRORLEVEL%
