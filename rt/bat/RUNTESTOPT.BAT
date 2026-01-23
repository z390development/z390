@if /I "%1" == "tron" (echo on) else (echo off)
rem validate detection of circular references in opt files

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

rem
echo %0: test 1 - should assemble okay
call bat\asm   %z_TraceMode% rt\mlc\testopt @rt\opt\testopt0.opt
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR on test 1: Encountered RC %z_ReturnCode% - exiting
                         set z_ReturnCode=8
                         goto return
                         )

rem
echo %0: test 2 - should assemble with error for invalid option
call bat\asm   %z_TraceMode% rt\mlc\testopt @rt\opt\testopt1.opt
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 16 (echo %0 ERROR on test 2: invalid option not detected
                          set z_ReturnCode=8
                          goto return
                          )

rem
echo %0: test 3 - should assemble okay with valid reference
call bat\asm   %z_TraceMode% rt\mlc\testopt @rt\opt\testopt2.opt
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR on test 3: Encountered RC %z_ReturnCode% - exiting
                         set z_ReturnCode=8
                         goto return
                         )

rem
echo %0: test 4 - should assemble with error for incorrect reference
call bat\asm   %z_TraceMode% rt\mlc\testopt @rt\opt\testopt3.opt
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 16 (echo %0 ERROR on test 4: incorrect reference not detected
                          set z_ReturnCode=8
                          goto return
                          )

rem
echo %0: test 5 - should assemble with error for self-reference
call bat\asm   %z_TraceMode% rt\mlc\testopt @rt\opt\testopt4.opt
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 16 (echo %0 ERROR on test 5: incorrect self-reference not detected
                          set z_ReturnCode=8
                          goto return
                          )

rem
echo %0: test 6 - should assemble with error for circular reference
call bat\asm   %z_TraceMode% rt\mlc\testopt @rt\opt\testopt5.opt
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 16 (echo %0 ERROR on test 6: circular reference not detected
                          set z_ReturnCode=8
                          goto return
                          )

rem
echo %0: all tests passed okay
set z_ReturnCode=0
goto return

:error
if %ERRORLEVEL% EQU 0 (set z_ReturnCode=6
               ) else (set z_ReturnCode=%ERRORLEVEL%)
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
