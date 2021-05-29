rem regression test z390 bmk Benchmark timing pgm by Melvyn Maltz
call rt\RTCLEAN1 bmk *
CALL ASMLG bmk\INSTTIME SYSPARM(bmk) ASCII MEM(5) %1 %2 %3
call rt\RTCOMP bmk rt\bmk\save rt\bmk\dif
call rt\RTLIST rt\bmk\dif
rem end of rtguam.bat
