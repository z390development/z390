rem test and verify mz390 only
copy test\junk.txt %1.bal
call MZ390 %1 notiming
call vdiff %1.bal %1.svb %1.dib 
