@if /I "%1" == "tron" (echo on) else (echo off)
rem test zREPRO LOAD/UNLOAD/VERIFY functions for zvsam(2)

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

:LOAD
rem LOAD THE VS-ESDS
echo --                      >>%data%\%logfile%
echo -- Step 1: load cluster >>%data%\%logfile%
echo --                      >>%data%\%logfile%
if exist %data%\MYFILE10.DTA del %data%\MYFILE10.DTA
if exist %data%\MYFILE10.IDX del %data%\MYFILE10.IDX
set INQSAM=%input%\MYFILEAV.TXT[RECFM=VT]
set OUTVSAM=%data%\Z390CAT2.MYFILE10
set Z390CAT2=%cat%\
call bat\EZ390 %code%\ZREPRO PARM(LOAD) DUMP zVSAM(2)
set retcode=%ERRORLEVEL%
type %code%\ZREPRO.LOG >>%data%\%logfile%
if %retcode% NEQ 0 goto error

:UNLOAD
rem UNLOAD THE VS-ESDS (ORIGINAL INPUT MYFILEAV.TXT)
echo --                        >>%data%\%logfile%
echo -- Step 2: unload cluster >>%data%\%logfile%
echo --                        >>%data%\%logfile%
set INVSAM=%data%\Z390CAT2.MYFILE10
set OUTQSAM=%data%\MYFILE10.TXO[RECFM=VT]
set Z390CAT2=%cat%
call BAT\EZ390 %code%\ZREPRO PARM(UNLOAD) DUMP zVSAM(2)
set retcode=%ERRORLEVEL%
type %code%\ZREPRO.LOG >>%data%\%logfile%
if %retcode% NEQ 0 goto error

:COMPARE
rem CHECK THE UNLOAD
echo --                             >>%data%\%logfile%
echo -- Step 3: compare unload data >>%data%\%logfile%
echo --                             >>%data%\%logfile%
set MYLOAD=%input%\MYFILEAV.TXT
set UNLOAD=%data%\MYFILE10.TXO
fc %MYLOAD% %UNLOAD% >>%data%\%logfile%
set retcode=%ERRORLEVEL%
if %retcode% NEQ 0 goto error

:VERIFY
rem VERIFY THE VS-ESDS
echo --                            >>%data%\%logfile%
echo -- Step 4: verify the cluster >>%data%\%logfile%
echo --                            >>%data%\%logfile%
set INVSAM=%data%\Z390CAT2.MYFILE10
set Z390CAT2=%cat%\
call BAT\EZ390 %code%\ZREPRO PARM(VERIFY) DUMP zVSAM(2)
set retcode=%ERRORLEVEL%
type %code%\ZREPRO.LOG >>%data%\%logfile%
if %retcode% NEQ 0 goto error

:BLDAIX
rem BUILDAIX UNIQUE MYAIXA1 OVER MYFILE10 VS-ESDS
echo --                               >>%data%\%logfile%
echo -- Step 5: build alternate index >>%data%\%logfile%
echo --                               >>%data%\%logfile%
if exist %data%\MYAIXA1.DTA del %data%\MYAIXA1.DTA >nul
if exist %data%\MYAIXA1.IDX del %data%\MYAIXA1.IDX >nul
set INVSAM=%data%\Z390CAT2.MYFILE10
set OUTVSAM=%data%\Z390CAT2.MYAIXA1
set Z390CAT2=%cat%\
call BAT\EZ390 %code%\ZREPRO PARM(BUILDAIX) NOTIME DUMP MEM(16) zVSAM(2)
set retcode=%ERRORLEVEL%
type %code%\ZREPRO.LOG >>%data%\%logfile%
if %retcode% NEQ 0 goto error

:BLDAIX2
rem BUILDAIX NON-UNIQUE MYAIXAU OVER MYFILE10 VS-ESDS
echo --                               >>%data%\%logfile%
echo -- Step 6: build alternate index >>%data%\%logfile%
echo --                               >>%data%\%logfile%
if exist %data%\MYAIXAU.DTA del %data%\MYAIXAU.DTA >nul
if exist %data%\MYAIXAU.IDX del %data%\MYAIXAU.IDX >nul
set INVSAM=%data%\Z390CAT2.MYFILE10
set OUTVSAM=%data%\Z390CAT2.MYAIXAU
set Z390CAT2=%cat%\
call BAT\EZ390 %code%\ZREPRO PARM(BUILDAIX) NOTIME DUMP MEM(16) zVSAM(2)
set retcode=%ERRORLEVEL%
type %code%\ZREPRO.LOG >>%data%\%logfile%
if %retcode% NEQ 0 goto error

echo --               >>%data%\%logfile%
echo -- Test complete >>%data%\%logfile%
echo --               >>%data%\%logfile%

set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%retcode%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
exit /b %z_ReturnCode%
