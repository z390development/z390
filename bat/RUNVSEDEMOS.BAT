rem run vse demos under z390
cd..
bat\asmlg vse\demo\DEMOVSE1 sysmac(vse\mac+mac)
pause verify DEMOVSE1 ended ok
bat\asmlg vse\demo\DEMOVSE2 sysmac(vse\mac+mac)
pause verify DEMOVSE2 ended ok
set SYSUT1=vse\demo\DEMOVSE3.TF1
set SYSUT2=vse\demo\DEMOVSE3.TF2
bat\asmlg vse\demo\DEMOVSE3 sysmac(vse\mac+mac)
pause verify DEMOVSE3 copied DEMOVSE3.TF1 to DEMOVSE3.TF2
