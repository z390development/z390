rem asmlg assemble, link, and go from mlc to 390 execution
erase %1.bal
erase %1.prn
erase %1.obj
erase %1.lst
erase %1.390
erase %1.log
erase %1.err
call %~dps0mz390 %1  sysmac(%~dps0mac+.) syscpy(%~dps0mac+.) %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto mzerr
call %~dps0lz390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto lzerr
call %~dps0ez390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto ezerr
goto end
:mzerr
pause see errors on mz390 generated bal file and console
goto end
:lzerr
pause see errors on lz390 generated lst file and console
erase %1.390
goto end
:ezerr
pause see errors on ez390 generated log file and console
:end 
