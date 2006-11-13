rem exec execute 390 load module
erase %1.log
erase %1.err
erase %1.tr*
call %~dps0ez390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto ezerr
goto end
:ezerr
pause see errors on ez390 generated log file and console
:end
