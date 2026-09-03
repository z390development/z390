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

rem LOAD THE V-ESDS
if exist vsam2\MYFILE01.DTA del vsam2\MYFILE01.DTA
if exist vsam2\MYFILE01.IDX del vsam2\MYFILE01.IDX
set INQSAM=cics\vsam\MYFILE1V.TXT[RECFM=VT]
set OUTVSAM=vsam2\Z390CAT2.MYFILE01
set Z390CAT2=vsam2\mlc\
call bat\EZ390 vsam2\mlc\ZREPRO PARM(LOAD) NOTIME DUMP zVSAM(2) || goto error
if exist vsam2\mlc\LOAD_ESDS_V.LOG del vsam2\mlc\LOAD_ESDS_V.LOG
ren vsam2\mlc\ZREPRO.LOG LOAD_ESDS_V.LOG
goto return


rem UNLOAD THE V-ESDS (ORIGINAL INPUT MYFILE1V.TXT)
set INVSAM=vsam2\Z390CAT2.MYFILE01
set OUTQSAM=vsam2\MYFILE1V.TXO[RECFM=VT]
set Z390CAT2=vsam2\mlc\
call BAT\EZ390 vsam2\mlc\ZREPRO PARM(UNLOAD) NOTIME DUMP
if errorlevel 1 goto WRONG

rem CHECK THE UNLOAD
set MYLOAD=vsam2\MYFILE1V.TXT
set UNLOAD=vsam2\MYFILE1V.TXO
call BAT\EZ390 vsam2\mlc\ZREPCOMP NOTIME DUMP
if errorlevel 1 goto WRONG
copy vsam2\mlc\MYFILE01.LOG+vsam2\mlc\ZREPRO.LOG+vsam2\mlc\ZREPCOMP.LOG vsam2\mlc\TEMP.LOG
del vsam2\mlc\MYFILE01.LOG
ren vsam2\mlc\TEMP.LOG MYFILE01.LOG

rem VERIFY THE V-ESDS
set INVSAM=vsam2\Z390CAT2.MYFILE01
set Z390CAT2=vsam2\mlc\
call BAT\EZ390 vsam2\mlc\ZREPRO PARM(VERIFY) NOTIME DUMP
if errorlevel 1 goto WRONG
copy vsam2\mlc\MYFILE01.LOG+vsam2\mlc\ZREPRO.LOG vsam2\mlc\TEMP.LOG
del  vsam2\mlc\MYFILE01.LOG
ren  vsam2\mlc\TEMP.LOG MYFILE01.LOG
MOVE /Y vsam2\mlc\MYFILE01.LOG vsam2
rem goto END

:DOAIX11
rem BUILDAIX UNIQUE MYAIX11 OVER MYFILE01 V-ESDS
del vsam2\MYAIX11.DTA
del vsam2\MYAIX11.IDX
set INVSAM=vsam2\Z390CAT2.MYFILE01
set OUTVSAM=vsam2\Z390CAT2.MYAIX11
set Z390CAT2=vsam2\mlc\
call BAT\EZ390 vsam2\mlc\ZREPRO PARM(BUILDAIX) NOTIME DUMP MEM(16)
if errorlevel 1 goto WRONG
del vsam2\mlc\MYAIX11.LOG
ren vsam2\mlc\ZREPRO.LOG MYAIX11.LOG
MOVE /Y vsam2\mlc\MYAIX11.LOG vsam2
rem goto END



set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
if exist vsam2\%SORTWK*.TMP del vsam2\SORTWK*.TMP
popd
exit /b %z_ReturnCode%
