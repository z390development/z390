@echo off
setlocal
pushd %~dp0
cd..
cd..
rem regression test z390 instructions
call bat\asm   tests\testins1 trace sysmac(mac) noloadhigh || goto error
call bat\asmlg tests\testins2 trace sysmac(mac) noloadhigh || goto error
call bat\asmlg tests\testins3 trace sysmac(mac) noloadhigh || goto error
call bat\asmlg tests\testins4 trace sysmac(mac) noloadhigh || goto error
call bat\asmlg tests\testins5 trace sysmac(mac) noloadhigh optable(z390) || goto error
call bat\asmlg tests\testdfp1 trace sysmac(mac) noloadhigh optable(z390) || goto error
call bat\asmlg tests\testdfp2 trace sysmac(mac) noloadhigh optable(z390) || goto error
call rt\bat\ZOPCHECK || goto errror
@REM pause verify tests ran without errors

goto:eof

:error
echo RUNASMTE.ERROR: Encountered RC %ERRORLEVEL% - exiting
exit /b %ERRORLEVEL%
