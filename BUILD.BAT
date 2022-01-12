@echo off
setlocal
pushd %~dp0

REM Run the tests
call bat\bldjar || goto error
call bat\bldlib || goto error
call bat\bldcbllib || goto error
call rt\bat\zopcheck || goto error
call bat\runasmdemos || goto error
call bat\runcbldemos || goto error
call rt\bat\runasmtests || goto error
call rt\bat\runcbltests || goto error
call rt\bat\runrtqsam TIME(30) || goto error
call rt\bat\runrtbsam || goto error
call rt\bat\runsort || goto error
call rt\bat\runzpar || goto error
call bat\runassist || goto error
call bat\runcmdproc || goto error
call bat\runvsedemos || goto error
call bat\runzstrmactest || goto error

goto:eof

:error
echo BUILD    ERROR: Encountered RC %ERRORLEVEL% - exiting
exit /b %ERRORLEVEL%
