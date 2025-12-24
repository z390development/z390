
@if /I "%1" == "tron" (echo on) else (echo off)
rem run z390 zcobol sample regression tests

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

rem -- *************************************************************
rem -- 
rem -- This script runs the z390/zCobol version of the NIST cobol test suite
rem -- which can be found in github repository z390development/nistcobol85
rem -- 
rem -- This bat script reflects the current status of the adoption
rem -- process of the NIST test suite in z390 / zCobol.
rem -- We have found 5 sets of the NIST test suite. None of them seem
rem -- to be ultimately authoritative as the NIST itself has abandoned
rem -- the entire test suite.
rem -- 
rem -- We are moving forward one program at a time, comparing the program
rem -- versions available and fixing any breakages in zCobol as we go.
rem -- That is, most compiles end with RC=8 - there are tons of issues
rem -- to be resolved. What we fix is where the compile process ends
rem -- abnormally, e.g. with a java exception and a stack trace.
rem -- 
rem -- Long story short: we reconcile source versions, and fix compiler breakage.
rem -- All other issues are left for future maintenance.
rem -- 
rem -- *************************************************************

pushd %~dps0..\..
if %1. == . goto no_repo

rem --
rem -- Copy members and purpose:
rem ALTL1.CPY - intended for use by EXEC85
rem ALTLB.CPY - intended for use by EXEC85

rem --
rem -- Communications Module
call bat\CBLCLG %z_TraceMode% %1\src\CM101M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\CM102M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\CM103M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\CM104M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\CM105M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\CM201M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\CM202M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\CM303M %2 %3 %4 %5 %6 %7 %8 %9
rem -- CM401M tests error conditions
call bat\CBLC   %z_TraceMode% %1\src\CM401M %2 %3 %4 %5 %6 %7 %8 %9

rem --
rem -- Debug module
call bat\CBLCLG %z_TraceMode% %1\src\DB101A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\DB102A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\DB103M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\DB104A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\DB105A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\DB201A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\DB202A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\DB203A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\DB204A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\DB205A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\DB301M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\DB302M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\DB303M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\DB304M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\DB305M %2 %3 %4 %5 %6 %7 %8 %9

rem --
rem -- Executive module
call bat\CBLCLG %z_TraceMode% %1\src\EXEC85 %2 %3 %4 %5 %6 %7 %8 %9

rem --
rem -- sort all error messages from all compiles
type %1\*.err >%1\$all.err
sort /+26 %1\$all.err /O %1\$allsort.err
echo .
echo All compiles completed
echo See %1\$allsort.err for complete list of all messages

set z_ReturnCode=0
goto return

:no_repo
set z_ReturnCode=8
echo %0 ERROR: Argument 1 should have been path to NIST repository - cannot run tests
:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
