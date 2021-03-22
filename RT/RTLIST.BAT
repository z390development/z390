rem display regression test %1 directory files in descending size order
if exist /usr/local/lib/z390/z390.jar goto linux
:windows
dir %1/o-s/p
goto end
:linux
dir %1
:end
