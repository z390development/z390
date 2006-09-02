rem mac run mz390 macro processor to expand mlc to bal with nomlc
erase %1.bal
erase %1.err
call %~dps0mz390 %1  bal noasm sysmac(%~dps0mac+.) syscpy(%~dps0mac+.) %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto err
goto end
:err
pause see mz390 bal errors
:end
