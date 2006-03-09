rem test and verify mz390 only
copy %~dps0test\junk.txt %1.bal
call %~dps0MZ390 %1 notiming
call %~dps0vdiff %1.bal %1.svb %1.dib 
