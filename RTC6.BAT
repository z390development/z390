rem test and verify asml only
erase %1.err
copy %~dps0test\junk.txt %1.bal
copy %~dps0test\junk.txt %1.obj
copy %~dps0test\junk.txt %1.prn
copy %~dps0test\junk.txt %1.lst
copy %~dps0test\junk.txt %1.390
call %~dps0asml %1 notiming %2 %3 %4 %5 %6 %7 %8 %9
call %~dps0vdiff %1.bal %1.svb  %1.dib 
call %~dps0vdiff %1.obj %1.svo  %1.dio 
call %~dps0vdiff %1.prn %1.svp  %1.dip 
call %~dps0vdiff %1.lst %1.svl  %1.dil 
call %~dps0vdiff %1.390 %1.sv3  %1.di3 
