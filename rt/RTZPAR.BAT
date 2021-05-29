rem run z390 zpar regression tests using saved tre files in rt\zpar
call rt\RTZPAR1 systerm(rt\zpar\rtzpar) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTLIST rt\zpar\dif 
rem end of rtzpar.bat
