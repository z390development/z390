@if /I "%1" == "tron" (echo on) else (echo off)
rem demo hello world demo

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
if /I "%1" == "*All" (set zcbl_all=yes
                      shift /1
              ) else (set zcbl_all=no)

rem
rem add Cobol sources are translated to mlc assembler source
rem during assembly some zcobol\mac directory is required 
rem for both copy members and macros
set maclib=zcobol\mac
set cpylib=zcobol\cpy
set cblopt=syscpy(+%maclib%\*.MAC+%cpylib%\*.CPY) sysmac(+%maclib%\*.MAC) NOTIME STATS

rem
rem The following programs compile and link okay
rem Some programs are compiled out of sequence
rem that is to avoid linkage issues

CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC102A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC101A %cblopt%

CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC104A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC105A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC103A %cblopt%

CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC107A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC106A %cblopt%

CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC111A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC110A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC109A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC108A %cblopt%

CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC113A %cblopt%

CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC118M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC117M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC116M %cblopt%

CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC202A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC204A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC206A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC211A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC212A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC214A %cblopt%

CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC217A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC216A %cblopt%

CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC110M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC111A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC113M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC124A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC126A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC127A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC131A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC133A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC135A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC136A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC137A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC140A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC141A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC210A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC220M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC239A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC240A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC241A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC248A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\OBIC3A %cblopt%
rem
rem The following programs compile okay but fail to link
rem IC114A calls IC115A
CALL bat\CBLC  %z_TraceMode% zcobol\NIST\IC114A %cblopt%
rem IC209A calls IC210A
CALL bat\CBLC  %z_TraceMode% zcobol\NIST\IC209A %cblopt%
rem IC213A calls IC215A
CALL bat\CBLC  %z_TraceMode% zcobol\NIST\IC213A %cblopt%
rem OBIC1A calls OBIC2A and OBIC3A
CALL bat\CBLC  %z_TraceMode% zcobol\NIST\OBIC1A %cblopt%
if /I %zcbl_all% NEQ yes goto return

rem
rem the following programs fail due to various reasons
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\CM101M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\CM102M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\CM103M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\CM103M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\CM104M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\CM105M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\CM201M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\CM202M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\DB101A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\DB102A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\DB103M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\DB104A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\DB105A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\DB201A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\DB202A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\DB203A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\DB204A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\DB205A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\EXEC85 %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC112A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC115A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC201A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC203A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC205A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC207A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC208A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC210A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IC215A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF101A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF102A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF103A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF104A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF105A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF106A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF107A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF108A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF109A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF110A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF111A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF112A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF113A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF114A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF115A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF116A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF117A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF118A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF119A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF120A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF121A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF122A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF123A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF124A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF125A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF126A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF127A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF128A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF129A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF130A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF131A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF132A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF133A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF134A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF135A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF136A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF137A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF138A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF139A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF140A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF141A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IF142A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX101A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX102A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX103A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX104A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX105A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX106A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX107A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX108A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX109A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX110A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX111A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX112A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX113A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX114A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX115A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX116A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX117A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX118A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX119A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX120A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX121A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX201A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX202A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX203A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX204A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX205A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX207A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX208A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX209A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX210A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX211A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX212A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX213A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX214A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX215A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX216A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX217A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\IX218A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC101A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC102A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC103A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC104A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC105A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC106A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC107A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC108M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC109M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC112A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC114M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC115A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC116A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC117A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC118A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC119A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC120A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC121M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC122A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC123A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC125A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC132A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC134A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC138A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC139A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC170A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC171A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC172A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC173A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC174A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC175A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC176A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC177A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC201A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC202A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC203A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC204M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC205A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC206A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC207A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC208A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC209A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC211A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC214M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC215A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC216A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC217A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC218A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC219A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC221A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC222A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC223A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC224A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC225A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC231A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC232A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC233A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC234A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC235A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC236A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC237A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC238A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC242A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC243A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC244A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC245A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC246A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC247A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC250A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC251A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC252A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC253A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\NC254A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\OBIC2A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\OBNC1M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\OBNC2M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\OBSQ1A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\OBSQ3A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\OBSQ4A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\OBSQ5A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL101A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL102A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL103A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL104A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL105A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL106A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL107A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL108A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL109A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL110A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL111A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL112A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL113A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL114A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL115A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL116A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL117A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL118A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL119A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL201A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL202A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL203A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL204A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL205A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL206A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL207A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL208A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL209A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL210A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL211A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL212A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RL213A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RW101A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RW102A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RW103A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\RW104A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SG101A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SG102A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SG103A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SG104A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SG105A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SG106A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SG201A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SG202A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SG203A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SG204A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SM101A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SM102A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SM103A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SM104A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SM105A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SM106A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SM107A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ101M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ102A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ103A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ104A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ105A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ106A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ107A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ108A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ109M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ110M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ111A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ112A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ113A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ114A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ115A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ116A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ117A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ121A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ122A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ123A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ124A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ125A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ126A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ127A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ128A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ129A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ130A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ131A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ132A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ133A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ134A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ135A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ136A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ137A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ138A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ139A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ140A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ141A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ142A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ143A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ144A %cblopt%
rem CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ145A %cblopt% missing
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ146A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ147A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ148A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ149A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ150A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ151A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ152A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ153A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ154A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ155A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ156A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ201M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ202A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ203A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ204A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ205A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ206A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ207M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ208M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ209M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ210M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ211A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ212A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ213A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ214A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ215A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ216A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ217A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ218A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ219A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ220A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ221A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ222A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ223A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ224A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ225A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ226A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ227A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ228A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ229A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\SQ230A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST101A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST102A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST103A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST104A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST105A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST106A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST107A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST108A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST109A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST110A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST111A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST112M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST113M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST114M %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST115A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST116A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST117A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST118A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST119A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST120A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST121A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST122A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST123A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST124A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST125A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST126A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST127A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST131A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST132A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST133A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST134A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST135A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST136A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST137A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST139A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST140A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST144A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST146A %cblopt%
CALL bat\CBLCL %z_TraceMode% zcobol\NIST\ST147A %cblopt%
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
