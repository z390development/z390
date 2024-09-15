@if /I "%1" == "tron" (echo on) else (echo off)
rem asm run macro assembly to generate relocatable obj from mlc source 

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

rem
rem TESTSORT = SORT ASCII 21 RECORD SIMPLE SORT
SET z_ReturnCode=0
SET SORTIN=sort\TESTSORT.IN[RECFM=FT,LRECL=80]
SET SORTOUT=sort\TESTSORT.OUT[RECMF=FT]
SET SYSIN=sort\TESTSORT.INI
if exist sort\TESTSORT.OUT erase sort\TESTSORT.OUT
if exist linklib\SORT.STA  erase linklib\SORT.STA
CALL bat\EXEC %z_TraceMode% linklib\SORT NOTIMING STATS %1 %2 %3 %4 %5
IF ERRORLEVEL 0 (echo **** No errors in SORT1 for TESTSORT.OUT
         ) ELSE (echo **** ++ Error! in SORT1 for TESTSORT.OUT
                 set  z_ReturnCode=8)
move linklib\SORT.STA sort\SORT1.STA >NUL
rem
rem TESTSRT1/2 TEST ALL KEY TYPES WITH 21 20 byte records
SET SYSUT2=sort\TESTSRT1.IN
CALL bat\ASMLG %z_TraceMode% sort\TESTSRT1 %1 %2 %3 %4 %5
IF ERRORLEVEL 0 (echo **** No errors in TESTSRT1 21 RECORD GEN
         ) ELSE (echo **** ++ Error! in TESTSRT1 21 RECORD GEN
                 set  z_ReturnCode=8)
rem
SET SORTIN=sort\TESTSRT1.IN[RECFM=F,LRECL=20]
SET SORTOUT=sort\TESTSRT1.OUT
SET SYSIN=sort\TESTSRT1.INI
if exist sort\TESTSRT1.OUT erase sort\TESTSRT1.OUT
CALL bat\EXEC %z_TraceMode% linklib\SORT STATS %1 %2 %3 %4 %5
IF ERRORLEVEL 0 (echo **** No errors in SORT2 for TESTSRT1.OUT
         ) ELSE (echo **** ++ Error! in SORT2 for TESTSRT1.OUT
                 set  z_ReturnCode=8)
move linklib\SORT.STA sort\SORT2.STA >NUL
rem
SET SYSUT1=sort\TESTSRT1.OUT
CALL bat\ASMLG %z_TraceMode% sort\TESTSRT2 %1 %2 %3 %4 %5
IF ERRORLEVEL 0 (echo **** No errors in TESTSRT2 verify TESTSRT1.OUT
         ) ELSE (echo **** ++ Error! in TESTSRT2 verify TESTSRT1.OUT
                 set  z_ReturnCode=8)
rem
rem TESTSRT3/4 TEST SORTING 100000 USING 1000 REC TABLE
SET SYSUT2=sort\TESTSRT3.IN
CALL bat\ASMLG %z_TraceMode% sort\TESTSRT3 PARM(100000) %1 %2 %3 %4 %5
IF ERRORLEVEL 0 (echo **** No errors in TESTSRT3 100000 record gen
         ) ELSE (echo **** ++ Error! in TESTSRT3 100000 record gen
                 set  z_ReturnCode=8)
rem
SET SORTIN=sort\TESTSRT3.IN[RECFM=F,LRECL=4]
SET SORTOUT=sort\TESTSRT3.OUT
SET SYSIN=sort\TESTSRT3.INI
if exist sort\TESTSRT3.OUT erase sort\TESTSRT3.OUT
CALL bat\EXEC %z_TraceMode% linklib\SORT STATS PARM(1000) %1 %2 %3 %4 %5
IF ERRORLEVEL 0 (echo **** No errors in SORT for TESTSRT3.OUT
         ) ELSE (echo **** ++ Error! in SORT for TESTSRT3.OUT
                 set  z_ReturnCode=8)
move linklib\SORT.STA sort\SORT3.STA >NUL
rem
SET SYSUT1=sort\TESTSRT3.OUT
CALL bat\ASMLG %z_TraceMode% sort\TESTSRT4 PARM(100000) %1 %2 %3 %4 %5
IF ERRORLEVEL 0 (echo **** No errors in TESTSRT4 verify TESTSRT3.OUT
         ) ELSE (echo **** ++ Error! in TESTSRT4 verify TESTSRT3.OUT
                 set  z_ReturnCode=8)
rem
if exist SORTWK*.TMP erase SORTWK*.TMP
if %z_ReturnCode% == 0 echo **** All SORT tests completed okay

:return
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
