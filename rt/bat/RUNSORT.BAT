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

rem SORT load module resides in linklib subdirectory!
set linklib=linklib
set sortlib=sort
set sys390=SYS390(%linklib%)

rem remove created files, if any, from previous SORT execution
if exist %sortlib%\SORT.ERR erase %sortlib%\SORT.ERR
if exist %sortlib%\SORT.LOG erase %sortlib%\SORT.LOG
if exist %sortlib%\SORT.STA erase %sortlib%\SORT.STA
rem Assemble test programs
call bat\ASML %z_TraceMode% %sortlib%\TESTSRT1 %1 %2 %3 %4 %5 || goto error
call bat\ASML %z_TraceMode% %sortlib%\TESTSRT2 %1 %2 %3 %4 %5 || goto error
call bat\ASML %z_TraceMode% %sortlib%\TESTSRT3 %1 %2 %3 %4 %5 || goto error
call bat\ASML %z_TraceMode% %sortlib%\TESTSRT4 %1 %2 %3 %4 %5 || goto error

rem Test 1: TESTSORT = sort ASCII 21 record simple sort
set SORTIN=%sortlib%\TESTSORT.IN[RECFM=FT,LRECL=80]
set SORTOUT=%sortlib%\TESTSORT.OUT
set SYSIN=%sortlib%\TESTSORT.INI
if exist %SORTOUT% erase %SORTOUT%
set SORTOUT=%SORTOUT%[RECFM=FT]
rem use bash/ez390 instead of bash/exec so SORT.ERR, SORT.LOG,
rem and SORT.STA are created in the sort directory
rem Note: SORT.390 is in linklib, not the sort directory
call bat\ez390 %z_TraceMode% %sortlib%\SORT %sys390% STATS %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
rename %sortlib%\SORT.STA SORT1.STA
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: SORT failure in sort test 1
                         goto return
                         )

rem Test 2: Test 2: TESTSRT1/2  Test all key types with 21 20 byte records
rem                     Generate data to sort
set SYSUT2=%sortlib%\TESTSRT1.IN
call bat\EXEC %z_TraceMode% %sortlib%\TESTSRT1 %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: TESTSRT1 failure generating data in sort test 2
                         goto return
                         )

rem Test 3: Test 3: TESTSRT1/2  Test all key types with 21 20 byte records
set SORTIN=%sortlib%\TESTSRT1.IN[RECFM=F,LRECL=20]
set SORTOUT=%sortlib%\TESTSRT1.OUT
set SYSIN=%sortlib%\TESTSRT1.INI
if exist %SORTOUT% erase %SORTOUT%
call bat\ez390 %z_TraceMode% %sortlib%\SORT %sys390% STATS %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
rename %sortlib%\SORT.STA SORT2.STA
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: SORT failure in sort test 3
                         goto return
                         )

rem Test 4: TESTSRT1/2  Test all key types with 21 20 byte records
rem                     Verify sorted file from TESTSRT1
set SYSUT1=%sortlib%\TESTSRT1.OUT
call bat\EXEC %z_TraceMode% %sortlib%\TESTSRT2 %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: test 4 TESTSRT2 failure verifying TESTSRT1.OUT from test 3
                         goto return
                         )

rem Test 5: TESTSRT3/4  Test sorting 100000 recs using 1000 rec table
rem                     Generate test file to be sorted;
rem
rem                       100,000 4-byte records:
rem
rem                       record #       record data
rem                       --------  ---------------------
rem                              1   100,000 = 0x000186A0
rem                              2    99,999 = 0x0001069F
rem                               ...
rem                        100,000         1 = 0x00000001
set SYSUT2=%sortlib%\TESTSRT3.IN
set parm=PARM(100000) TIME(300)
call bat\EXEC %z_TraceMode% %sortlib%\TESTSRT3 %parm% %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: TESTSRT3 failure generating data in test 5
                         goto return
                         )

rem Test 6: TESTSRT3/4  Test sorting 100000 recs using 1000 rec table
rem                     Sort the file generated in test 5
set SORTIN=%sortlib%\TESTSRT3.IN[RECFM=F,LRECL=4]
set SORTOUT=%sortlib%\TESTSRT3.OUT
set SYSIN=%sortlib%\TESTSRT3.INI
if exist %SORTOUT% erase %SORTOUT%
set sortparm=PARM(1000) TIME(300)
call bat\ez390 %z_TraceMode% %sortlib%\SORT %sys390% %sortparm% %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
rename %sortlib%\SORT.STA SORT3.STA
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: SORT failure in test 6
                         goto return
                         )

rem Test 7: TESTSRT3/4  Test sorting 100000 recs using 1000 rec table
rem                     Verify sorted file from TESTSRT3
set SYSUT1=%sortlib%\TESTSRT3.OUT
call bat\EXEC %z_TraceMode% %sortlib%\TESTSRT4 %parm% %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: test 7 TESTSRT4 failure verifying TESTSRT3.OUT from test 6
                         goto return
                         )
rem done with tests

rem remove residual files from SORT executions
if exist %sortlib%\SORT.ERR erase %sortlib%\SORT.ERR
if exist %sortlib%\SORT.LOG erase %sortlib%\SORT.LOG
if exist %sortlib%\SORT.STA erase %sortlib%\SORT.STA
if exist        SORTWK*.TMP erase SORTWK*.TMP

rem if we get here, there were no errors
echo Verify all sort tests ok
goto return

:error
echo %0 ERROR: Test program failed to assemble. See log and listings for details.

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
