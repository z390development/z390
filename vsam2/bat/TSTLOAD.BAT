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

call vsam2\bat\TSTLOAD01.BAT    || goto error && rem ESDS-V  + AIX
call vsam2\bat\TSTLOAD02.BAT    || goto error && rem ESDS-F  + AIX
call vsam2\bat\TSTLOAD03.BAT    || goto error && rem KSDS-V  + AIX
call vsam2\bat\TSTLOAD06.BAT    || goto error && rem RRDS-F
call vsam2\bat\TSTLOAD07.BAT    || goto error && rem RRDS-V
call vsam2\bat\TSTLOAD08.BAT    || goto error && rem KSDS-F
call vsam2\bat\TSTLOAD09.BAT    || goto error && rem KSDS-F  + AIX
call vsam2\bat\TSTLOADIN.BAT %1 || goto error && rem KSDS-F  + AIX (test 2 spacemap pages)
call vsam2\bat\TSTLOAD10.BAT    || goto error && rem ESDS-VS + AIX
call vsam2\bat\TSTLOAD11.BAT    || goto error && rem RRDS-VS
call vsam2\bat\TSTLOAD12.BAT    || goto error && rem KSDS-VS + AIX
call vsam2\bat\TSTLOAD13.BAT    || goto error && rem KSDS-FS + AIX
call vsam2\bat\TSTLOADNK.BAT    || goto error && rem KSDS-F  + AIX
call vsam2\bat\TSTLOADNE.BAT    || goto error && rem ESDS-F
call vsam2\bat\TSTLOADNR.BAT    || goto error && rem RRDS-F

set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
exit /b %z_ReturnCode%
