rem vdiff compare %1 with %2 creating %3 dif file
if exist %1 goto ok1
pause vdiff file missing %1 
:ok1
if exist %2 goto ok2
pause vdiff file missing %1 
:ok2
fc %1 %2 > %3
rem if errorlevel 1 pause vdiff error
