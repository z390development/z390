rem compare %1 test files with %2 save files and gen %3 dif files
dir %1 > %~dps0rtgendr1.dat
call mac linklib\rtgendir sysparm(%~dps0rtgendr1)
if errorlevel 1 pause error in rtgendir processing %~dps0rtgendr1.dat
dir %2 > %~dps0rtgendr2.dat
call mac linklib\rtgendir sysparm(%~dps0rtgendr2)
if errorlevel 1 pause error in rtgendir processing %~dps0rtgendr2.dat
copy %~dps0rtgendr1.pch+%~dps0rtgendr2.pch %~dps0rtgendr3.dat
if errorlevel 1 pause error on copy for %~dps0rtgendr1+%~dps0rtgendr2 to %~dps0rtgendr3.dat
sort < %~dps0rtgendr3.dat > %~dps0rtgencmp.dat
if errorlevel 1 pause error on sort of %~dps0rtgendr3.dat to %~dps0rtgencmp.dat
call mac linklib\rtgencmp sysparm(%~dps0rtgencmp)
if errorlevel 1 pause error in rtgencmp processing %~dps0rtgencmp.dat
erase  %~dps0rtgencmp.bat
rename %~dps0rtgencmp.pch rtgencmp.bat 
call %~dps0rtgencmp.bat %1 %2 %3
