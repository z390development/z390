@if /I "%1" == "tron" (echo on) else (echo off)
rem rebuild guam demo and test programs

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

pushd %~dps0..
call bat\asml %z_TraceMode% guam\demo\DEMOAPL1  sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% guam\demo\DEMOGUI1  sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% guam\demo\DEMOGUI2  sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% guam\demo\DEMOGUI3  sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% guam\demo\DEMOGUI4  sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% guam\demo\DEMOGUI5  sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% guam\demo\DEMOGUI6  sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% guam\demo\HELLO1    sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% guam\demo\HELLO2    sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% guam\demo\HELLO3    sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error

call bat\asml %z_TraceMode% guam\test\AID       sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% guam\test\EXSAMPL2  sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% guam\test\TESTGP1   sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% guam\test\TESTGP2   sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% guam\test\TESTGP3   sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% guam\test\TESTGP4   sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\asml %z_TraceMode% guam\test\TGUAM     sysmac(mac) syscpy(mac) guam %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
