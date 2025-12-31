
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
rem -- Inter-Program Communication - subprograms must be compiled before the main program!
del %1\src\IC*.OBJ >NUL                                             || rem clear out leftovers of prior run
set XXXXX055=%1\src\IC101A_XXXXX055.OUT
call bat\CBLC   %z_TraceMode% %1\src\IC102A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IC101A %2 %3 %4 %5 %6 %7 %8 %9
set XXXXX055=%1\src\IC103A_XXXXX055.OUT
call bat\CBLC   %z_TraceMode% %1\src\IC104A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLC   %z_TraceMode% %1\src\IC105A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IC103A %2 %3 %4 %5 %6 %7 %8 %9
set XXXXX055=%1\src\IC106A_XXXXX055.OUT
call bat\CBLC   %z_TraceMode% %1\src\IC107A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCL  %z_TraceMode% %1\src\IC106A %2 %3 %4 %5 %6 %7 %8 %9 || rem execution ends S0C7
set XXXXX055=%1\src\IC108A_XXXXX055.OUT
call bat\CBLC   %z_TraceMode% %1\src\IC109A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLC   %z_TraceMode% %1\src\IC110A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLC   %z_TraceMode% %1\src\IC111A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IC108A %2 %3 %4 %5 %6 %7 %8 %9
set XXXXX055=%1\src\IC112A_XXXXX055.OUT
call bat\CBLC   %z_TraceMode% %1\src\IC113A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IC112A %2 %3 %4 %5 %6 %7 %8 %9 || rem fails to generate the call to IC113A
set XXXXX055=%1\src\IC114A_XXXXX055.OUT
call bat\CBLC   %z_TraceMode% %1\src\IC115A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLC   %z_TraceMode% %1\src\IC114A %2 %3 %4 %5 %6 %7 %8 %9 || rem fails to link because of errors in IC115A
set XXXXX055=%1\src\IC116A_XXXXX055.OUT
call bat\CBLC   %z_TraceMode% %1\src\IC117M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLC   %z_TraceMode% %1\src\IC118M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IC116M %2 %3 %4 %5 %6 %7 %8 %9
set XXXXX055=%1\src\IC201A_XXXXX055.OUT
call bat\CBLC   %z_TraceMode% %1\src\IC202A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IC201A %2 %3 %4 %5 %6 %7 %8 %9
set XXXXX055=%1\src\IC203A_XXXXX055.OUT
call bat\CBLC   %z_TraceMode% %1\src\IC204A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLC   %z_TraceMode% %1\src\IC205A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLC   %z_TraceMode% %1\src\IC206A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IC203A %2 %3 %4 %5 %6 %7 %8 %9
set XXXXX055=%1\src\IC207A_XXXXX055.OUT
call bat\CBLC   %z_TraceMode% %1\src\IC208A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IC207A %2 %3 %4 %5 %6 %7 %8 %9
set XXXXX055=%1\src\IC209A_XXXXX055.OUT
call bat\CBLC   %z_TraceMode% %1\src\IC210A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLC   %z_TraceMode% %1\src\IC211A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLC   %z_TraceMode% %1\src\IC212A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLC   %z_TraceMode% %1\src\IC209A %2 %3 %4 %5 %6 %7 %8 %9 || rem fails to link because of errors in IC209A
set XXXXX055=%1\src\IC213A_XXXXX055.OUT
call bat\CBLC   %z_TraceMode% %1\src\IC214A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLC   %z_TraceMode% %1\src\IC215A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLC   %z_TraceMode% %1\src\IC213A %2 %3 %4 %5 %6 %7 %8 %9 || rem fails to link because of errors in IC215A
set XXXXX055=%1\src\IC216A_XXXXX055.OUT
call bat\CBLC   %z_TraceMode% %1\src\IC217A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IC216A %2 %3 %4 %5 %6 %7 %8 %9
set XXXXX055=
rem  bat\CBLCLG %z_TraceMode% %1\src\IC222A %2 %3 %4 %5 %6 %7 %8 %9 || rem uses multiple programs in 1 source file
rem  bat\CBLCLG %z_TraceMode% %1\src\IC223A %2 %3 %4 %5 %6 %7 %8 %9 || rem uses multiple programs in 1 source file
rem  bat\CBLCLG %z_TraceMode% %1\src\IC224A %2 %3 %4 %5 %6 %7 %8 %9 || rem uses multiple programs in 1 source file
rem  bat\CBLCLG %z_TraceMode% %1\src\IC225A %2 %3 %4 %5 %6 %7 %8 %9 || rem uses multiple programs in 1 source file
rem  bat\CBLCLG %z_TraceMode% %1\src\IC226A %2 %3 %4 %5 %6 %7 %8 %9 || rem uses multiple programs in 1 source file
rem  bat\CBLCLG %z_TraceMode% %1\src\IC227A %2 %3 %4 %5 %6 %7 %8 %9 || rem uses multiple programs in 1 source file
rem  bat\CBLCLG %z_TraceMode% %1\src\IC228A %2 %3 %4 %5 %6 %7 %8 %9 || rem uses multiple programs in 1 source file
rem  bat\CBLCLG %z_TraceMode% %1\src\IC233A %2 %3 %4 %5 %6 %7 %8 %9 || rem uses multiple programs in 1 source file
rem  bat\CBLCLG %z_TraceMode% %1\src\IC234A %2 %3 %4 %5 %6 %7 %8 %9 || rem uses multiple programs in 1 source file
rem  bat\CBLCLG %z_TraceMode% %1\src\IC235A %2 %3 %4 %5 %6 %7 %8 %9 || rem uses multiple programs in 1 source file
rem  bat\CBLCLG %z_TraceMode% %1\src\IC237A %2 %3 %4 %5 %6 %7 %8 %9 || rem uses multiple programs in 1 source file
rem  bat\CBLCLG %z_TraceMode% %1\src\IC401A %2 %3 %4 %5 %6 %7 %8 %9 || rem uses multiple programs in 1 source file

rem --
rem -- sort all error messages from all compiles
type %1\src\*.err >%1\src\$all.err 2>nul
sort /+26 %1\src\$all.err /O %1\src\$allsort.err
echo .
echo All compiles completed
echo See %1\src\$allsort.err for complete list of all messages

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
