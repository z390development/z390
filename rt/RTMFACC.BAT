rem run z390 MFACC regression tests using mfacc dir files
rem rtcomp - compare mfacc files with mfacc\save files to build dif files
rem rtlist - display mfacc\dif dir in descending size order
call rt\RTCLEAN1 mfacc
call rt\RTMFACC1 systerm(mfacc\rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTLIST rt\mfacc\dif 
rem end of mfacc.bat
