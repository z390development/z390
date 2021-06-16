rem run vse demos under z390
rem 2021/06/09 dsh fix issue #244 by adding "call "
@setlocal
@pushd %~dp0
cd..
call bat\asmlg vse\demo\DEMOVSE1 sysmac(vse\mac+mac)
@REM pause verify DEMOVSE1 ended ok
call bat\asmlg vse\demo\DEMOVSE2 sysmac(vse\mac+mac)
@REM pause verify DEMOVSE2 ended ok
set SYSUT1=vse\demo\DEMOVSE3.TF1
set SYSUT2=vse\demo\DEMOVSE3.TF2
call bat\asmlg vse\demo\DEMOVSE3 sysmac(vse\mac+mac)
@REM pause verify DEMOVSE3 copied DEMOVSE3.TF1 to DEMOVSE3.TF2
