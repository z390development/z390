rem remove gnerated files from directories except cics, linklib
erase cics\*.tr*
erase cics\*.prn
erase linklib\*.tr*
erase linklib\*.prn
call rt\RTCLEAN1 assist
call rt\RTCLEAN1 bmk
call rt\RTCLEAN1 demo
call rt\RTCLEAN1 guam
call rt\RTCLEAN1 guam\demo
call rt\RTCLEAN1 guam\test
call rt\RTCLEAN1 mac
call rt\RTCLEAN1 mfacc
call rt\RTCLEAN1 mvs
call rt\RTCLEAN1 mvs\demo
call rt\RTCLEAN1 mvs\test
call rt\RTCLEAN1 pc
call rt\RTCLEAN1 rt
call rt\RTCLEAN1 rt\test
call rt\RTCLEAN1 soa
call rt\RTCLEAN1 soa\demo
call rt\RTCLEAN1 soa\test
call rt\RTCLEAN1 soa\maclib
call rt\RTCLEAN1 sort
call rt\RTCLEAN1 vsam
call rt\RTCLEAN1 vsam\demo
call rt\RTCLEAN1 vsam\test
call rt\RTCLEAN1 vse
call rt\RTCLEAN1 vse\mac
call rt\RTCLEAN1 vse\demo
call rt\RTCLEAN1 vse\test
call rt\RTCLEAN1 zcobol
call rt\RTCLEAN1 zcobol\bat
call rt\RTCLEAN1 zcobol\demo
call rt\RTCLEAN1 zcobol\test
call rt\RTCLEAN1 zcobol\test\z390
call rt\RTCLEAN1 zcobol\test\java
call rt\RTCLEAN1 zcobol\test\vce
call rt\RTCLEAN1 zcobol\test\i586
call rt\RTCLEAN1 zcobol\z390
call rt\RTCLEAN1 zcobol\java
call rt\RTCLEAN1 zcobol\vce
call rt\RTCLEAN1 zcobol\i586
call rt\RTCLEAN1 linklib\barcode
call rt\RTCLEAN1 zpar
erase zpar\*.dat
erase zpar\*.rp*
erase zpar\*.tm*
