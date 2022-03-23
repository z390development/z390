@if /I "%1" == "tron" (echo on) else (echo off)
rem regression test IEFBR14 Okay / Warning / Error variants

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
call bat\asm   %z_TraceMode% rt\mlc\IEFBR14O || goto error
call bat\asml  %z_TraceMode% rt\mlc\IEFBR14O || goto error
call bat\asmlg %z_TraceMode% rt\mlc\IEFBR14O || goto error
call bat\asm   %z_TraceMode% rt\mlc\IEFBR14W
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 4 (echo %0 ERROR: IEFBR14W did not end with RC=4; RC=%z_ReturnCode%
                         set z_ReturnCode=8
                         goto return)
call bat\asml  %z_TraceMode% rt\mlc\IEFBR14W || goto error
call bat\asmlg %z_TraceMode% rt\mlc\IEFBR14W || goto error
call bat\asm   %z_TraceMode% rt\mlc\IEFBR14E
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 12 (echo %0 ERROR: IEFBR14E did not end with RC=12; RC=%z_ReturnCode%
                          set z_ReturnCode=8
                          goto return)
call bat\asml  %z_TraceMode% rt\mlc\IEFBR14E
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 12 (echo %0 ERROR: IEFBR14E did not end with RC=12; RC=%z_ReturnCode%
                          set z_ReturnCode=8
                          goto return)
call bat\asmlg %z_TraceMode% rt\mlc\IEFBR14E
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 12 (echo %0 ERROR: IEFBR14E did not end with RC=12; RC=%z_ReturnCode%
                          set z_ReturnCode=8
                          goto return)
set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
