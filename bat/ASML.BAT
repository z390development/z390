@if /I "%1" == "tron" (echo on) else (echo off)
rem asml assemble and link to generate load module 390 from mlc

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

if /I %1. == . (set /P z_file=Hit Enter for help or suppply name of mlc file to assemble and link: 
        ) else (set z_file=%1)
if /I %z_file%. == . goto help
:chkfile
rem convert Unix path notation to Windows before checking the file
set z_file=%z_file:/=\%
if not exist %z_file%.MLC (echo %0 ERROR: %z_file%.MLC was not found
                           set z_ReturnCode=16
                           goto return)
if exist %z_file%.BAL erase %z_file%.BAL
if exist %z_file%.PRN erase %z_file%.PRN
if exist %z_file%.OBJ erase %z_file%.OBJ
if exist %z_file%.LST erase %z_file%.LST
if exist %z_file%.390 erase %z_file%.390
if exist %z_file%.LOG erase %z_file%.LOG
if exist %z_file%.ERR erase %z_file%.ERR
if exist %z_file%.STA erase %z_file%.STA
if exist %z_file%.TR* erase %z_file%.TR*

rem get the z390 directory
set "z_HomeDir=%~dps0..\"
for %%f in ("%z_HomeDir%") do set "z_HomeDir=%%~ff"

call %~dps0mz390 %z_TraceMode% %z_file% sysmac(+%z_HomeDir%mac) syscpy(+%z_HomeDir%mac) %2 %3 %4 %5 %6 %7 %8 %9
set z_ReturnCode=%ERRORLEVEL%
rem any error or warning message has been issued by mz390
if %z_ReturnCode% NEQ 0 (if %z_ReturnCode% NEQ 4 (erase %1.OBJ
                                                  goto return))

call %~dps0lz390 %z_TraceMode% %z_file% %2 %3 %4 %5 %6 %7 %8 %9
set z_ReturnCode=%ERRORLEVEL%
rem any error or warning message has been issued by lz390
if %z_ReturnCode% NEQ 0 (if %z_ReturnCode% NEQ 4 (erase %1.390
                                                  goto return))

:return
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%

:help
echo .
echo the ASML.BAT procedure is intended to assemble and link a single assembler program
echo .
set /P z_file=Hit Enter for more help or suppply name of mlc file to assemble and link: 
if /I %z_file%. NEQ . goto chkfile

call %~dps0help %z_TraceMode%
set z_ReturnCode=16
goto return
