rem assemble and interactively test CICS compatible transaction manager
erase cics\dif\*.* /Q
call RTCLEAN1 cics
rem assemble and link all programs in cics
call cics\DFHALL stats
rem start the server as a separate process (note 2 local terminals start)
start cics\Z390CICG
pause start a remote terminal session (use server IP)
call  cics\Z390KCPR 192.168.1.5
call RTCOMP cics cics\save cics\dif
call RTLIST cics\dif
rem end of interactive cics test
