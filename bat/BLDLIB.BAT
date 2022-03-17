@if /I "%1" == "tron" (echo on) else (echo off)
rem rebuild z390 linklib modules

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
call bat\asml  %z_TraceMode% linklib\REPRO    bal notiming stats       || goto error
call bat\asml  %z_TraceMode% linklib\SUPERZAP bal notiming stats       || goto error
call bat\asml  %z_TraceMode% linklib\UNREF    bal notiming stats ascii || goto error
call bat\asm   %z_TraceMode% linklib\FPCONVRT bal notiming stats       || goto error
call bat\asm   %z_TraceMode% linklib\FPCONMFC bal notiming stats       || goto error
call bat\asm   %z_TraceMode% linklib\CVTTOHEX bal notiming stats       || goto error
call bat\asm   %z_TraceMode% linklib\SQXTR    bal notiming stats       || goto error
call bat\asm   %z_TraceMode% linklib\DAT      bal notiming stats       || goto error
call bat\asmlg %z_TraceMode% linklib\IEFBR14  sysobj(+linklib)         || goto error
call bat\asmlg %z_TraceMode% linklib\TESTFPC1 sysobj(+linklib)         || goto error
call bat\asmlg %z_TraceMode% linklib\TESTFPC2 sysobj(+linklib)         || goto error

call bat\asm   %z_TraceMode% linklib\RTGENDIR noasm bal notiming stats sysparm(RTGENDIR) || goto error
call bat\asm   %z_TraceMode% linklib\RTGENCMP noasm bal notiming stats sysparm(RTGENCMP) || goto error
set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
