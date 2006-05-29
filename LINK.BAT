rem link to generate 390 load module from 1 or more obj files
erase %1.390
erase %1.lst
erase %1.err
call %~dps0lz390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto lzerr
goto end
:lzerr
pause see errors on lz390 generated lst file and console
:end
