rem display regression test %1 directory files in descending size order
if exist /usr/local/lib/z390/z390.jar goto linux
:windows
dir %1 > %1\RTGENDIF.DAT
call mac linklib\RTGENDIF sysparm(%1\RTGENDIF) notime %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 pause error in RTGENDIF checking dif files in %1
call  %1\RTGENDIF.BAT %2 %3 %4 %5 %6 %7 %8 %9
erase %1\RTGEN*.*
dir %1/o-s/p
goto end
:linux
dir %1
:end
