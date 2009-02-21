rem remove gnerated files from directories except cics, linklib
call rt\RTCLEAN1 zcobol
call rt\RTCLEAN1 zcobol\bat
call rt\RTCLEAN1 zcobol\cics
call rt\RTCLEAN1 zcobol\demo
call rt\RTCLEAN1 zcobol\test
rem zcobol demo save files for each target type
call rt\RTCLEAN1 zcobol\demo\z390
call rt\RTCLEAN1 zcobol\demo\java
call rt\RTCLEAN1 zcobol\demo\vce
call rt\RTCLEAN1 zcobol\demo\i586
rem zcobol test save files for each target type
call rt\RTCLEAN1 zcobol\test\z390
call rt\RTCLEAN1 zcobol\test\java
call rt\RTCLEAN1 zcobol\test\vce
call rt\RTCLEAN1 zcobol\test\i586
rem zcobol generation macro directories for each target type
call rt\RTCLEAN1 zcobol\z390
call rt\RTCLEAN1 zcobol\java
call rt\RTCLEAN1 zcobol\vce
call rt\RTCLEAN1 zcobol\i586
