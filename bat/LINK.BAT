@if /I "%1" == "tron" (echo on) else (echo off)
rem link to generate 390 load module from 1 or more obj files

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

if /I %1. == . (set /P z_file=Hit Enter for help or suppply name of obj file to link: 
        ) else (set z_file=%1)
if /I %z_file%. == . goto help
:chkfile
rem convert Unix path notation to Windows before checking the file
set z_file=%z_file:/=\%
if exist %z_file%.OBJ goto file_ok
rem if user specified valid extension, strip it off
if /I %z_file:~-4%. NEQ .OBJ. goto help
set z_file=%z_file:~0,-4%
if exist %z_file%.OBJ goto file_ok
echo %0 ERROR: %z_file%.OBJ was not found
set z_ReturnCode=16
goto return

:file_ok
if exist %z_file%.390 erase %z_file%.390
if exist %z_file%.LST erase %z_file%.LST
if exist %z_file%.ERR erase %z_file%.ERR
if exist %z_file%.STA erase %z_file%.STA
if exist %z_file%.TR* erase %z_file%.TR*
call %~dps0LZ390 %z_TraceMode% %z_file% %2 %3 %4 %5 %6 %7 %8 %9
set z_ReturnCode=%ERRORLEVEL%
rem any error or warning message has been issued by lz390
if %z_ReturnCode% NEQ 0 (if %z_ReturnCode% NEQ 4 (erase %z_file%.390
                                                  goto return))

:return
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%

:help
echo .
echo the LINK.BAT procedure is intended to link a single program
echo .
set /P z_file=Hit Enter for more help or suppply name of obj file to link: 
if /I %z_file%. NEQ . goto chkfile

call %~dps0help %z_TraceMode%
set z_ReturnCode=16
goto return
