echo on
rem compare %1 test files with %2 save files and gen %3 dif files
if exist /usr/local/lib/z390/z390.jar goto linux
:windows
dir %1 > %~dps0RTGENDR1.DAT
call mac linklib\RTGENDIR sysparm(%~dps0RTGENDR1)
if errorlevel 1 pause error in rtgendir processing %~dps0RTGENDR1.DAT
dir %2 > %~dps0RTGENDR2.DAT
call mac linklib\RTGENDIR sysparm(%~dps0RTGENDR2)
if errorlevel 1 pause error in rtgendir processing %~dps0RTGENDR2.DAT
copy %~dps0RTGENDR1.pch+%~dps0RTGENDR2.PCH %~dps0RTGENDR3.DAT
if errorlevel 1 pause error on copy for %~dps0RTGENDR1+%~dps0RTGENDR2 to %~dps0RTGENDR3.DAT
sort < %~dps0RTGENDR3.DAT > %~dps0RTGENCMP.DAT
if errorlevel 1 pause error on sort of %~dps0RTGENDR3.DAT to %~dps0RTGENCMP.DAT
call mac linklib\RTGENCMP sysparm(%~dps0RTGENCMP) 
if errorlevel 1 pause error in rtgencmp processing %~dps0RTGENCMP.DAT
erase  %~dps0RTGENCMP.BAT
rename %~dps0RTGENCMP.PCH RTGENCMP.BAT 
call %~dps0RTGENCMP.BAT %1 %2 %3
goto end
:linux
diff %1 %2 %3/DIFF.TXT
:end
