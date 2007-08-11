rem run rtvsam pgms and gen rt\dif
erase %~dps0vsam\demo\dif\*.* /q
call RTCLEAN1 vsam\demo *
rem build vsam cluster definition tables
call ASML  %~dps0vsam\demo\ESF1     notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASML  %~dps0vsam\demo\ESV1     notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASML  %~dps0vsam\demo\RRF1     notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASML  %~dps0vsam\demo\LDF1     notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call ASML  %~dps0vsam\demo\KSF1     notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
rem create and read back ESF1 ESDS fixed length file using ESF1SP and ESF1SG 
set  INFILE=vsam\demo\ESF1.TF1
set  OUTFILE=vsam\demo\ESF1.390
call ASMLG %~dps0vsam\demo\ESF1SP   trace stats notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
set  INFILE=vsam\demo\ESF1.390
set  OUTFILE=vsam\demo\ESF1.TF2
call ASMLG %~dps0vsam\demo\ESF1SG   trace stats notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
rem create and read back ESF1 ESDS using REPRO utility
set  INFILE=vsam\demo\ESF1.TF1
set  OUTFILE=vsam\demo\ESF1.390
call ASMLG linklib\REPRO         bal trace stats notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
copy linklib\REPRO.LOG vsam\demo\repro1.LOG
copy linklib\REPRO.TRE vsam\demo\repro1.TRE
set  INFILE=vsam\demo\ESF1.390
set  OUTFILE=vsam\demo\ESF1.TF2
call EXEC linklib\REPRO            trace stats notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
copy linklib\REPRO.LOG vsam\demo\repro2.LOG
copy linklib\REPRO.TRE vsam\demo\repro2.TRE
rem create and read back ESV1 ESDS var len file using ESV1SP and ESV1SG 
set  OUTFILE=vsam\demo\ESV1.390
call ASMLG %~dps0vsam\demo\ESV1SP   trace stats notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
set  INFILE=vsam\demo\ESV1.390
call ASMLG %~dps0vsam\demo\ESV1SG   notiming %1 %2 %3 %4 %5 %6 %7 %8 %9
call RTCOMP %~dps0vsam\demo %~dps0vsam\demo\save %~dps0vsam\demo\dif
