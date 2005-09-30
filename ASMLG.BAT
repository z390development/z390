rem asmlg assemble, link, and go from mlc to 390 execution
call mz390 %1 %2 %3 %4 %5 %6 %7 %8 %9 sysmac(mac) syscpy(mac)
if errorlevel 1 goto mzerr
call az390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto azerr
call lz390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto lzerr
call ez390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto ezerr
goto end
:mzerr
rem see errors on mz390 generated bal file and console
goto end
:azerr
rem see errors on az390 generated prn file and console
goto end
:lzerr
rem see errors on lz390 generated lst file and console
goto end
:ezerr
rem see errors on ez390 generated log file and console
:end
