rem testand verify compile, link, execute, mlc (no pch or data files)
copy test\junk.txt %1.bal
copy test\junk.txt %1.obj
copy test\junk.txt %1.prn
copy test\junk.txt %1.lst
copy test\junk.txt %1.390
copy test\junk.txt %1.log
call asmlg %1 notiming %2 %3 %4 %5
call vdiff %1.bal %1.svb  %1.dib 
call vdiff %1.obj %1.svo  %1.dio 
call vdiff %1.prn %1.svp  %1.dip 
call vdiff %1.lst %1.svl  %1.dil 
call vdiff %1.390 %1.sv3  %1.di3 
call vdiff %1.log %1.svg  %1.dig 
