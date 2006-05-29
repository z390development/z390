rem test and verify mz390 only
erase %1.err
copy %~dps0test\junk.txt %1.bal
call %~dps0MZ390 %1 notiming %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0vdiff %1.bal %1.svb %1.dib 
