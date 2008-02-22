rem run mfacc pgms and gen mfacc\dif
rem rt1 - assemble, link, and execute with notiming
rem rtcomp - compare mfacc files with mfacc\save files to build mfacc\dif
rem rtlist - display mfacc\dif dir in descending size order
call rt\RTCLEAN1 mfacc *
rem
rem assemble, link, and execute demo wto 'hello world'
rem
call rt\RT1 mfacc\P10DSH1   systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P10DSH2   systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P10MB1    systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P11MODEL  systerm(rtmfacc) SYSCPY(mfacc+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P11FIND1  systerm(rtmfacc) SYSCPY(mfacc+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P11DSH1   systerm(rtmfacc) SYSCPY(mfacc+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P11FIND2  systerm(rtmfacc) SYSCPY(mfacc+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P11DSH2   systerm(rtmfacc) SYSCPY(mfacc+mac) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P12DSH1   systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P12DSH2   systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P12DSH3   systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P13SC1    systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P13DSH1   systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P14MW1    systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P14DSH1   systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P15DSH1   systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P16DSH1   systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P1C1      systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P1DSH1    systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P1DSH1A   systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P1RAFA1   systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P2MD1     systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P3LKM1    systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P3MM1     systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P4AN1     systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P4APN2    systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P4DSH1    systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P4RAFA2   systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P4RJ1     systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P5MM1     systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P6BR1     systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P6MM1     systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P6PJF1    systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P6PL1     systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P6RW1     systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P7DSH1    systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P7EH1     systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P7RV1     systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P8DSH1    systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P8DSH2    systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P8LM1     systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P8MM1     systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P9DSH1    systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P9MM1     systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RT1 mfacc\P9MM2     systerm(rtmfacc) %1 %2 %3 %4 %5 %6 %7 %8 %9
call rt\RTCOMP mfacc rt\mfacc\save rt\mfacc\dif
