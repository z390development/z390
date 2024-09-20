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

rem Assemble test programs
call bat\ASML %z_TraceMode% %sortlib%\TESTSRT1 %1 %2 %3 %4 %5 || goto error
call bat\ASML %z_TraceMode% %sortlib%\TESTSRT2 %1 %2 %3 %4 %5 || goto error
call bat\ASML %z_TraceMode% %sortlib%\TESTSRT3 %1 %2 %3 %4 %5 || goto error
call bat\ASML %z_TraceMode% %sortlib%\TESTSRT4 %1 %2 %3 %4 %5 || goto error

rem Test 1: TESTSORT = SORT ASCII 21 RECORD SIMPLE SORT
set SORTIN=%sortlib%\TESTSORT.IN[RECFM=FT,LRECL=80]
set SORTOUT=%sortlib%\TESTSORT.OUT[RECMF=FT]
set SYSIN=%sortlib%\TESTSORT.INI
if exist %sortlib%\TESTSORT.OUT erase %sortlib%\TESTSORT.OUT
call bat\EXEC %z_TraceMode% %linklib%\SORT STATS(%sortlib%) SYSERR(%sortlib%) SYSLOG(%sortlib%) %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
rename %sortlib%\SORT.STA SORT1.STA
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: sort failure in sort test 1
                         goto return
                         )

rem Test 2: TESTSRT1/2 TEST ALL KEY TYPES WITH 21 20 byte records
set SYSUT2=%sortlib%\TESTSRT1.IN
call bat\EXEC %z_TraceMode% %sortlib%\TESTSRT1 %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: sort failure in sort test 2
                         goto return
                         )

rem Test 3: TESTSRT1/2 TEST ALL KEY TYPES WITH 21 20 byte records
set SORTIN=%sortlib%\TESTSRT1.IN[RECFM=F,LRECL=20]
set SORTOUT=%sortlib%\TESTSRT1.OUT
set SYSIN=%sortlib%\TESTSRT1.INI
if exist %sortlib%\TESTSRT1.OUT erase %sortlib%\TESTSRT1.OUT
call bat\EXEC %z_TraceMode% %linklib%\SORT STATS(%sortlib%) SYSERR(%sortlib%) SYSLOG(%sortlib%) %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
rename %sortlib%\SORT.STA SORT2.STA
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: sort failure in sort test 3
                         goto return
                         )

rem Test 4: TESTSRT1/2 TEST ALL KEY TYPES WITH 21 20 byte records
set SYSUT1=%sortlib%\TESTSRT1.OUT
call bat\EXEC %z_TraceMode% %sortlib%\TESTSRT2 %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: sort failure in sort test 4 - verify TESTSRT1.OUT
                         goto return
                         )

rem Test 5: TESTSRT3/4 TEST SORTING 100000 USING 1000 REC TABLE
set SYSUT2=%sortlib%\TESTSRT3.IN
call bat\EXEC %z_TraceMode% %sortlib%\TESTSRT3 PARM(100000) %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: sort failure in sort test 5
                         goto return
                         )

rem Test 6: TESTSRT3/4 TEST SORTING 100000 USING 1000 REC TABLE
set SORTIN=%sortlib%\TESTSRT3.IN[RECFM=F,LRECL=4]
set SORTOUT=%sortlib%\TESTSRT3.OUT
set SYSIN=%sortlib%\TESTSRT3.INI
if exist %sortlib%\TESTSRT3.OUT erase %sortlib%\TESTSRT3.OUT
call bat\EXEC %z_TraceMode% %linklib%\SORT STATS(%sortlib%) SYSERR(%sortlib%) SYSLOG(%sortlib%) PARM(1000) %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
rename %sortlib%\SORT.STA SORT3.STA
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: sort failure in sort test 6 - verify TESTSRT3.OUT
                         goto return
                         )

rem Test 7: TESTSRT3/4 TEST SORTING 100000 USING 1000 REC TABLE
set SYSUT1=%sortlib%\TESTSRT3.OUT
call bat\EXEC %z_TraceMode% %sortlib%\TESTSRT4 PARM(100000) %1 %2 %3 %4 %5
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: sort failure in sort test 7 - verify TESTSRT3.OUT
                         goto return
                         )
goto return

:error
echo %0 ERROR: Test program failed to assemble. See log and listings for details.

:return
if %z_ReturnCode% EQU 0 if exist %sortlib%\SORT.ERR erase %sortlib%\SORT.ERR
if %z_ReturnCode% EQU 0 if exist %sortlib%\SORT.LOG erase %sortlib%\SORT.LOG
if %z_ReturnCode% EQU 0 if exist %sortlib%\SORT.STA erase %sortlib%\SORT.STA
                        if exist        SORTWK*.TMP erase SORTWK*.TMP
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
