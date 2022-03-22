@if /I "%1" == "tron" (echo on) else (echo off)
rem ZPARTRS.BAT COBOL AND ASSEMBLER SOURCE TRACE FOR %1 DIR\PGMNAME
rem PGMNAME MUST HAVE BEEN COMPILED AND EXECUTED TO GEN PRN, LST, AND TRE FILES

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

if /I %1. == . (echo %0 ERROR: missing file name
                set z_ReturnCode=16
                goto return
                )

erase %1.TRS
call %~dps0MAC %z_TraceMode% zpar\ZPARTRS SYSPARM(%1+%2+%3+%4+%5+%6+%7+%8+%9) @ZPAR\ZPARTRS
set z_ReturnCode=%ERRORLEVEL%
rem any error or warning message has been issued by mac
if %z_ReturnCode% NEQ 0 (if %z_ReturnCode% NEQ 4 (goto return))

dir %1.*

:return
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
