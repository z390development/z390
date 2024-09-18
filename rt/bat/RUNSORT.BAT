@if /I "%1" == "tron" (echo on) else (echo off)
rem rebuild and test sort utility

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

pushd %~dps0..\..

rem Copy sort load module
copy linklib\SORT.390 sort\SORT.390
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: sort load module not copied
                         goto return
                         )

rem Assemble test programs
call bat\ASML %z_TraceMode% sort\TESTSRT1 %1 %2 %3 %4 %5 || goto error
call bat\ASML %z_TraceMode% sort\TESTSRT2 %1 %2 %3 %4 %5 || goto error
call bat\ASML %z_TraceMode% sort\TESTSRT3 %1 %2 %3 %4 %5 || goto error
call bat\ASML %z_TraceMode% sort\TESTSRT4 %1 %2 %3 %4 %5 || goto error

rem Test 1: TESTSORT = SORT ASCII 21 RECORD SIMPLE SORT
set SORTIN=sort\TESTSORT.IN[RECFM=FT,LRECL=80]
set SORTOUT=sort\TESTSORT.OUT[RECMF=FT]
set SYSIN=sort\TESTSORT.INI
if exist sort\TESTSORT.OUT erase sort\TESTSORT.OUT
call bat\EXEC %z_TraceMode% sort\SORT STATS %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: sort failure in sort test 1
                         copy sort\SORT.STA sort\SORT1.STA
                         goto return
                         )

rem Test 2: TESTSRT1/2 TEST ALL KEY TYPES WITH 21 20 byte records
set SYSUT2=sort\TESTSRT1.IN
call bat\EXEC %z_TraceMode% sort\TESTSRT1 %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: sort failure in sort test 2
                         goto return
                         )

rem Test 3: TESTSRT1/2 TEST ALL KEY TYPES WITH 21 20 byte records
set SORTIN=sort\TESTSRT1.IN[RECFM=F,LRECL=20]
set SORTOUT=sort\TESTSRT1.OUT
set SYSIN=sort\TESTSRT1.INI
if exist sort\TESTSRT1.OUT erase sort\TESTSRT1.OUT
call bat\EXEC %z_TraceMode% sort\SORT STATS %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: sort failure in sort test 3
                         goto return
                         )

rem Test 4: TESTSRT1/2 TEST ALL KEY TYPES WITH 21 20 byte records
copy sort\SORT.STA sort\SORT2.STA
set SYSUT1=sort\TESTSRT1.OUT
call bat\EXEC %z_TraceMode% sort\TESTSRT2 %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: sort failure in sort test 4 - verify TESTSRT1.OUT
                         goto return
                         )

rem Test 5: TESTSRT3/4 TEST SORTING 100000 USING 1000 REC TABLE
set SYSUT2=sort\TESTSRT3.IN
call bat\EXEC %z_TraceMode% sort\TESTSRT3 PARM(100000) %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: sort failure in sort test 5
                         goto return
                         )

rem Test 6: TESTSRT3/4 TEST SORTING 100000 USING 1000 REC TABLE
set SORTIN=sort\TESTSRT3.IN[RECFM=F,LRECL=4]
set SORTOUT=sort\TESTSRT3.OUT
set SYSIN=sort\TESTSRT3.INI
if exist sort\TESTSRT3.OUT erase sort\TESTSRT3.OUT
call bat\EXEC %z_TraceMode% sort\SORT STATS PARM(1000) %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: sort failure in sort test 6 - verify TESTSRT3.OUT
                         goto return
                         )

rem Test 7: TESTSRT3/4 TEST SORTING 100000 USING 1000 REC TABLE
copy sort\SORT.STA sort\SORT3.STA
set SYSUT1=sort\TESTSRT3.OUT
call bat\EXEC %z_TraceMode% sort\TESTSRT4 PARM(100000) %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: sort failure in sort test 7 - verify TESTSRT3.OUT
                         goto return
                         )
goto return

:error
echo %0 ERROR: Test program failed to assemble. See log and listings for details.

:return
if exist sort\SORT.390 erase sort\SORT.390

popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
