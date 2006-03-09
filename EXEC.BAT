rem exec execute 390 load module
call %~dps0ez390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto ezerr
goto end
:ezerr
rem see errors on ez390 generated log file and console
:end
