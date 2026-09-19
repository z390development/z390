@if /I "%1" == "tron" (echo on) else (echo off)
rem test zREPRO LOAD/UNLOAD/VERIFY functions for zvsam(2)
rem Argument 1 is expected to point to a local clone of the z390Archive repository

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

call vsam2\bat\TSTLOAD01.BAT %z_TraceMode%       || goto error && rem ESDS-V  + AIX
call vsam2\bat\TSTLOAD02.BAT %z_TraceMode%       || goto error && rem ESDS-F  + AIX
call vsam2\bat\TSTLOAD03.BAT %z_TraceMode%       || goto error && rem KSDS-V  + AIX
call vsam2\bat\TSTLOAD06.BAT %z_TraceMode%       || goto error && rem RRDS-F
call vsam2\bat\TSTLOAD07.BAT %z_TraceMode%       || goto error && rem RRDS-V
call vsam2\bat\TSTLOAD08.BAT %z_TraceMode%       || goto error && rem KSDS-F
call vsam2\bat\TSTLOAD09.BAT %z_TraceMode%       || goto error && rem KSDS-F  + AIX
call vsam2\bat\TSTLOADIN.BAT %z_TraceMode% "%~1" || goto error && rem KSDS-F  + AIX (test 2 spacemap pages)
call vsam2\bat\TSTLOAD10.BAT %z_TraceMode%       || goto error && rem ESDS-VS + AIX
call vsam2\bat\TSTLOAD11.BAT %z_TraceMode%       || goto error && rem RRDS-VS
call vsam2\bat\TSTLOAD12.BAT %z_TraceMode%       || goto error && rem KSDS-VS + AIX
call vsam2\bat\TSTLOAD13.BAT %z_TraceMode%       || goto error && rem KSDS-FS + AIX
call vsam2\bat\TSTLOADNK.BAT %z_TraceMode%       || goto error && rem KSDS-F  + AIX
call vsam2\bat\TSTLOADNE.BAT %z_TraceMode%       || goto error && rem ESDS-F
call vsam2\bat\TSTLOADNR.BAT %z_TraceMode%       || goto error && rem RRDS-F

set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
exit /b %z_ReturnCode%
