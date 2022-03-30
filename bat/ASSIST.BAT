@if /I "%1" == "tron" (echo on) else (echo off)
rem Assemble, link, and exec ASSIST program %1

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

if /I %1. == . (set /P z_file=Hit Enter for help or suppply name of mlc file to assemble, link, and execute: 
        ) else (set z_file=%1)
if /I %z_file%. == . goto help
:chkfile
rem convert Unix path notation to Windows before checking the file
set z_file=%z_file:/=\%
if not exist %z_file%.MLC (echo %0 ERROR: %z_file%.MLC was not found
                           set z_ReturnCode=16
                           goto return)

set XREAD=%z_file%.XRD
set XPRNT=%z_file%.XPR
set XPNCH=%z_file%.XPH
set XGET=%z_file%.XGT
set XPUT=%z_file%.XPT
call %~dps0ASMLG %z_TraceMode% %z_file% assist %2 %3 %4 %5 %6 %7 %8 %9
set z_ReturnCode=%ERRORLEVEL%
rem any error or warning message has been issued by asmlg

:return
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%

:help
echo .
echo the ASSIST.BAT procedure is intended to assemble, link, and execute a single assembler program
echo .
set /P z_file=Hit Enter for more help or suppply name of mlc file to assemble, link, and execute: 
if /I %z_file%. NEQ . goto chkfile

call %~dps0help %z_TraceMode%
set z_ReturnCode=16
goto return
