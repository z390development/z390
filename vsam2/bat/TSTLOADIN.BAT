@if /I "%1" == "tron" (echo on) else (echo off)
rem assemble, link test programs and files for zvsam(2)

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
set input=vsam2\data
set data=vsam2\data
set cat=vsam2\mlc
set code=vsam2\mlc

set logfile=%~n0.LOG
echo %0 >%data%\%logfile%

rem
rem Argument 1 is expected to point to a local clone of the z390Archive repository
rem
if /I %1. == . goto noArchive
if not exist %1\TestData\COMPANIES.TXT goto noArchive
set input=%1\TestData
goto LOAD
:NoArchive
echo no path to z390Archive clone specified, or path was not valid
echo defaulting to use the small subset at %input%

:LOAD
echo Input data: %input%\COMPANIES.TXT >>%data%\%logfile%
rem LOAD THE F-KSDS
echo --                      >>%data%\%logfile%
echo -- Step 1: load cluster >>%data%\%logfile%
echo --                      >>%data%\%logfile%
if exist %data%\INDIANCO.DTA del %data%\INDIANCO.DTA
if exist %data%\INDIANCO.IDX del %data%\INDIANCO.IDX
set INQSAM=%input%\COMPANIES.TXT[RECFM=FT]
set OUTVSAM=%data%\Z390CAT2.INDIANCO
set Z390CAT2=%cat%\
call bat\EZ390 %code%\ZREPRO PARM(LOAD) NOTIME MAXSIZE(1000) DUMP zVSAM(2)
set retcode=%ERRORLEVEL%
type %code%\ZREPRO.LOG >>%data%\%logfile%
if %retcode% NEQ 0 goto error

:UNLOAD
rem UNLOAD THE F-KSDS (ORIGINAL INPUT COMPANIES.TXT from z390Archive repo)
echo --                        >>%data%\%logfile%
echo -- Step 2: unload cluster >>%data%\%logfile%
echo --                        >>%data%\%logfile%
set INVSAM=%data%\Z390CAT2.INDIANCO
set OUTQSAM=%data%\COMPANIES.TXO[RECFM=FT]
set Z390CAT2=%cat%
call BAT\EZ390 %code%\ZREPRO PARM(UNLOAD) NOTIME MAXSIZE(1000) DUMP zVSAM(2)
set retcode=%ERRORLEVEL%
type %code%\ZREPRO.LOG >>%data%\%logfile%
if %retcode% NEQ 0 goto error

:COMPARE
rem CHECK THE UNLOAD
echo --                             >>%data%\%logfile%
echo -- Step 3: compare unload data >>%data%\%logfile%
echo --                             >>%data%\%logfile%
set MYLOAD=%input%\COMPANIES.TXT
set UNLOAD=%data%\COMPANIES.TXO
fc %MYLOAD% %UNLOAD% >>%data%\%logfile%
set retcode=%ERRORLEVEL%
if %retcode% NEQ 0 goto error

:VERIFY
rem VERIFY THE F-KSDS
echo --                            >>%data%\%logfile%
echo -- Step 4: verify the cluster >>%data%\%logfile%
echo --                            >>%data%\%logfile%
set INVSAM=%data%\Z390CAT2.INDIANCO
set Z390CAT2=%cat%\
call BAT\EZ390 %code%\ZREPRO PARM(VERIFY) NOTIME DUMP zVSAM(2)
set retcode=%ERRORLEVEL%
type %code%\ZREPRO.LOG >>%data%\%logfile%
if %retcode% NEQ 0 goto error

:BLDAIX
rem BUILDAIX NON-UNIQUE MYAIXIN OVER INDIANCO F-KSDS
echo --                               >>%data%\%logfile%
echo -- Step 5: build alternate index >>%data%\%logfile%
echo --                               >>%data%\%logfile%
if exist %data%\MYAIXIN.DTA del %data%\MYAIXIN.DTA >nul
if exist %data%\MYAIXIN.IDX del %data%\MYAIXIN.IDX >nul
set INVSAM=%data%\Z390CAT2.INDIANCO
set OUTVSAM=%data%\Z390CAT2.MYAIXIN
set Z390CAT2=%cat%\
call BAT\EZ390 %code%\ZREPRO PARM(BUILDAIX) NOTIME DUMP MEM(16) MAXSIZE(1000) zVSAM(2)
set retcode=%ERRORLEVEL%
type %code%\ZREPRO.LOG >>%data%\%logfile%
if %retcode% NEQ 0 goto error

:UNLOADAIX
rem UNLOAD THE AIX AS A VS-KSDS
echo --                            >>%data%\%logfile%
echo -- Step 6: unload aix cluster >>%data%\%logfile%
echo --                            >>%data%\%logfile%
rem -- MYAIXKS defines the AIX cluster MYAIXIN as a plain KSDS
set INVSAM=%data%\Z390CAT2.MYAIXKS
set OUTQSAM=%data%\MYAIXKS.TXO[RECFM=VT]
set Z390CAT2=%cat%
call BAT\EZ390 %code%\ZREPRO PARM(UNLOAD) NOTIME DUMP zVSAM(2)
set retcode=%ERRORLEVEL%
type %code%\ZREPRO.LOG >>%data%\%logfile%
if %retcode% NEQ 0 goto error

:VERIFYAIX
rem VERIFY THE AIX VS-KSDS
echo --                            >>%data%\%logfile%
echo -- Step 7: verify the cluster >>%data%\%logfile%
echo --                            >>%data%\%logfile%
set INVSAM=%data%\Z390CAT2.MYAIXKS
set Z390CAT2=%cat%\
call BAT\EZ390 %code%\ZREPRO PARM(VERIFY) NOTIME DUMP zVSAM(2)
set retcode=%ERRORLEVEL%
type %code%\ZREPRO.LOG >>%data%\%logfile%
if %retcode% NEQ 0 goto error

echo --               >>%data%\%logfile%
echo -- Test complete >>%data%\%logfile%
echo --               >>%data%\%logfile%

set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
exit /b %z_ReturnCode%
