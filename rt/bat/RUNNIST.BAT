
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
set z_cleanup=N
if /I %1. EQU cleanup. (set z_cleanup=Y
                        shift /1)
if /I %2. EQU cleanup. (set z_cleanup=Y
                        shift /2)
if %1. == . goto no_repo

rem --
rem -- clear leftovers from prior runs
echo Cleaning up...
if exist %1\src\*.390 del %1\src\*.390
if exist %1\src\*.BAL del %1\src\*.BAL
if exist %1\src\*.ERR del %1\src\*.ERR
if exist %1\src\*.LOG del %1\src\*.LOG
if exist %1\src\*.LST del %1\src\*.LST
if exist %1\src\*.MLC del %1\src\*.MLC
if exist %1\src\*.OBJ del %1\src\*.OBJ
if exist %1\src\*.OUT del %1\src\*.OUT
if exist %1\src\*.PRN del %1\src\*.PRN
if exist %1\src\*.CBL_ZC_LABELS.CPY del %1\src\*.CBL_ZC_LABELS.CPY
echo Cleanup complete
if /I %z_cleanup%. EQU Y. goto return

rem --
rem -- Copy members and purpose:
rem ALTL1.CPY - intended for use by EXEC85
rem ALTLB.CPY - intended for use by EXEC85

rem --
rem -- CM = Communications Module
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
rem -- DB = Debug module
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
rem -- EX = Executive module
call bat\CBLCLG %z_TraceMode% %1\src\EXEC85 %2 %3 %4 %5 %6 %7 %8 %9

rem --
rem -- IC = Inter-Program Communication - subprograms must be compiled before each main program!
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
rem -- IF = Intrinsic Functions
call bat\CBLCLG %z_TraceMode% %1\src\IF101A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF102A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF103A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF104A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF105A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF106A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF107A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF108A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF109A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF110A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF111A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF112A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF113A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF114A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF115A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF116A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF117A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF118A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF119A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF120A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF121A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF122A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF123A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF124A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF125A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF126A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF127A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF128A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF129A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF130A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF131A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF132A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF133A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF134A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF135A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF136A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF137A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF138A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF139A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF140A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF141A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF142A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF401M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF402M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IF403M %2 %3 %4 %5 %6 %7 %8 %9

rem --
rem -- IX = Indexed I/O
call bat\CBLCLG %z_TraceMode% %1\src\IX101A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX102A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX103A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX104A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX105A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX106A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX107A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX108A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX109A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX110A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX111A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX112A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX113A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX114A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX115A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX116A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX117A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX118A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX119A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX120A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX121A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX201A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX202A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX203A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX204A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX205A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX206A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX207A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX208A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX209A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX210A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX211A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX212A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX213A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX214A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX215A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX216A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX217A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX218A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX301M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX302M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\IX401M %2 %3 %4 %5 %6 %7 %8 %9

rem --
rem -- Copy members and purpose:
rem K101A.CPY intended usage unknown
rem K1DAA.CPY intended usage unknown
rem K1FDA.CPY intended usage unknown
rem K1P01.CPY intended usage unknown
rem K1PRA.CPY intended usage unknown
rem K1PRB.CPY intended usage unknown
rem K1PRC.CPY intended usage unknown
rem K1SEA.CPY intended usage unknown
rem K1W01.CPY intended usage unknown
rem K1W02.CPY intended usage unknown
rem K1W03.CPY intended usage unknown
rem K1W04.CPY intended usage unknown
rem K1WKA.CPY intended usage unknown
rem K1WKB.CPY intended usage unknown
rem K1WKC.CPY intended usage unknown
rem K1WKY.CPY intended usage unknown
rem K1WKZ.CPY intended usage unknown
rem K2PRA.CPY intended usage unknown
rem K2SEA.CPY intended usage unknown
rem K3FCA.CPY intended usage unknown
rem K3FCB.CPY intended usage unknown
rem K3IOA.CPY intended usage unknown
rem K3IOB.CPY intended usage unknown
rem K3LGE.CPY intended usage unknown
rem K3OCA.CPY intended usage unknown
rem K3SCA.CPY intended usage unknown
rem K3SML.CPY intended usage unknown
rem K3SNA.CPY intended usage unknown
rem K3SNB.CPY intended usage unknown
rem K4NTA.CPY intended usage unknown
rem K501A.CPY intended usage unknown
rem K501B.CPY intended usage unknown
rem K5SDA.CPY intended usage unknown
rem K5SDB.CPY intended usage unknown
rem K6SCA.CPY intended usage unknown
rem K7SEA.CPY intended usage unknown
rem KK208A.CPY intended usage unknown
rem KP001.CPY intended usage unknown
rem KP002.CPY intended usage unknown
rem KP003.CPY intended usage unknown
rem KP004.CPY intended usage unknown
rem KP005.CPY intended usage unknown
rem KP006.CPY intended usage unknown
rem KP007.CPY intended usage unknown
rem KP008.CPY intended usage unknown
rem KP009.CPY intended usage unknown
rem KP010.CPY intended usage unknown
rem KSM31.CPY intended usage unknown
rem KSM41.CPY intended usage unknown

rem --
rem -- NC = Nucleus
call bat\CBLCLG %z_TraceMode% %1\src\NC101A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\NC102A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\NC103A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\NC104A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\NC105A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\NC106A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\NC107A %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\NC108M %2 %3 %4 %5 %6 %7 %8 %9
call bat\CBLCLG %z_TraceMode% %1\src\NC109M %2 %3 %4 %5 %6 %7 %8 %9

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
