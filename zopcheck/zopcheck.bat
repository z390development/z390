cd..
rem regression test z390 instructions
call bat\asmlg zopcheck\zopcheck bal trace sysmac(mac) syscpy(zopcheck+mac) optable(z390)
pause verify zopcheck ran without errors