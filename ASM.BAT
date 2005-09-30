rem asm run macro assembly to generate relocatible obj from mlc source 
call mz390 %1 %2 %3 %4 %5 %6 %7 %8 %9 sysmac(mac) syscpy(mac)
if errorlevel 1 goto mzerr
call az390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto azerr
goto end
:mzerr
rem see errors on mz390 generated bal file and console
goto end
:azerr
rem see errors on az390 generated prn file and console
:end
