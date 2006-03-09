REM test and verify file i/o from sysut1=%1.tf1 and sysut2=%2.tf2
set sysut1=%1.tf1
set sysut2=%1.tf2
set sysout=%1.tf3
copy %~dps0test\junk.txt %1.tf2
copy %~dps0test\junk.txt %1.tf3
copy %~dps0test\junk.txt %1.bal
copy %~dps0test\junk.txt %1.obj
copy %~dps0test\junk.txt %1.prn
copy %~dps0test\junk.txt %1.lst
copy %~dps0test\junk.txt %1.390
copy %~dps0test\junk.txt %1.log
copy %~dps0test\junk.txt %1.pch
call %~dps0asmlg %1 notiming %2 %3 %4 %5
call %~dps0vdiff %1.bal %1.svb  %1.dib 
call %~dps0vdiff %1.obj %1.svo  %1.dio 
call %~dps0vdiff %1.prn %1.svp  %1.dip 
call %~dps0vdiff %1.lst %1.svl  %1.dil 
call %~dps0vdiff %1.390 %1.sv3  %1.di3 
call %~dps0vdiff %1.log %1.svg  %1.dig 
if exist %1.svh goto :chkpch
goto :skppch
:chkpch
call %~dps0vdiff %1.pch %1.svh  %1.dih 
:skppch
if exist %1.sf1 goto :chktf1
goto :skptf1
:chktf1
call %~dps0vdiff %1.tf1 %1.sf1  %1.df1 
:skptf1
if exist %1.sf2 goto :chktf2
goto :skptf2
:chktf2
call %~dps0vdiff %1.tf2 %1.sf2  %1.df2 
:skptf2
if exist %1.sf3 goto :chktf3
goto :skptf3
:chktf3
call %~dps0vdiff %1.tf3 %1.sf3  %1.df3 
:skptf3
