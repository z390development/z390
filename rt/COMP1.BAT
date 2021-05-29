rem COMP1 compare %1 with %2 creating %3 dif file and set RC
if exist %1 goto ok1
pause vdiff file missing %1
:ok1
if exist %2 goto ok2
pause vdiff file missing %2
goto end 
:ok2
if exist /usr/local/lib/z390/z390.jar goto linux
:windows
fc %1 %2 > %3
goto end
:linux
rem note following diff cmd is pre-processed by cmd.pl
diff %1 %2 %3
:end
