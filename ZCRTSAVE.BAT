rem save files for ZCRT390 ZCRTJAV ZCRTVCE and ZCRT586
erase zcobol\demo\z390\*.bak /q
erase zcobol\demo\z390\save\*.* /q
copy  zcobol\demo\z390\*.* zcobol\demo\z390\save 
erase zcobol\demo\java\*.bak /q
erase zcobol\demo\java\save\*.* /q
copy  zcobol\demo\java\*.* zcobol\demo\java\save 
erase zcobol\demo\vce\*.bak /q
erase zcobol\demo\vce\save\*.* /q
copy  zcobol\demo\vce\*.* zcobol\demo\vce\save 
erase zcobol\demo\i586\*.bak /q
erase zcobol\demo\i586\save\*.* /q
copy  zcobol\demo\i586\*.* zcobol\demo\i586\save 
rem save test files for ZCRTTEST
erase zcobol\test\z390\*.bak 
erase zcobol\test\z390\save\*.* /q
copy  zcobol\test\z390\*.* zcobol\test\z390\save 
erase zcobol\test\java\*.bak 
erase zcobol\test\java\save\*.* /q
copy  zcobol\test\java\*.* zcobol\test\java\save 
erase zcobol\test\vce\*.bak
erase zcobol\test\vce\save\*.*  /q
copy  zcobol\test\vce\*.* zcobol\test\vce\save 
erase zcobol\test\i586\*.bak 
erase zcobol\test\i586\save\*.* /q
copy  zcobol\test\i586\*.* zcobol\test\i586\save 
rem end of ZCRTTSAV
