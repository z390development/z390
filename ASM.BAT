rem asm run macro assembly to generate relocatible obj from mlc source 
erase %1.bal
erase %1.prn
erase %1.obj
erase %1.err
call %~dps0mz390 %1  sysmac(%~dps0mac+.) syscpy(%~dps0mac+.) %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto mzerr
goto end
:mzerr
erase %1.obj
pause see errors on mz390 generated bal file and console
:end
