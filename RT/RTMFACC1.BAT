rem run mfacc pgms and gen mfacc\dif
rem rt1 - assemble, link, and execute with notiming
rem rtcomp - compare mfacc files with mfacc\save files to build mfacc\dif
rem rtlist - display mfacc\dif dir in descending size order
call rt\RTCLEAN1 mfacc *
rem
rem assemble, link, and execute demo wto 'hello world'
rem
call rt\RT1 mfacc\P10DSH1    %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P10DSH2    %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P10MB1     %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P11MODEL   SYSCPY(mfacc+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P11FIND1   SYSCPY(mfacc+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P11DSH1    SYSCPY(mfacc+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P11FIND2   SYSCPY(mfacc+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P11DSH2    SYSCPY(mfacc+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P12DSH1    %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P12DSH2    %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P12DSH3    %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P13SC1     %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P13DSH1    %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P14MW1     %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P14DSH1    %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P15DSH1    %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P16WR1     %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P16DSH1    %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P17WR1     %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P1C1       %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P1DSH1     %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P1DSH1A    %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P1RAFA1    %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P2MD1      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P3LKM1     %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P3MM1      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P4AN1      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P4APN2     %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P4DSH1     %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P4RAFA2    %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P4RJ1      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P5MM1      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P6BR1      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P6MM1      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P6PJF1     %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P6PL1      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P6RW1      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P7DSH1     %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P7EH1      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P7RV1      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P8DSH1     %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P8DSH2     %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P8LM1      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P8MM1      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P9DSH1     %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P9MM1      %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P9MM2      %1 %2 %3 %4 %5 %6 %7 %8 %9
erase rt\mfacc\dif\*.* /q
call rt\RTCOMP mfacc rt\mfacc\save rt\mfacc\dif
