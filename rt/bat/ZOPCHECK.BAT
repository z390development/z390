@setlocal
@pushd %~dp0
cd..\..
rem regression test z390 instructions
set SNAPOUT=ZOPCHECK\SNAPOUT.TXT
call bat\asmlg zopcheck\ZOPCHECK bal trace sysmac(mac) syscpy(zopcheck+mac) optable(z390)
@REM pause verify zopcheck ran without errors