@if /I "%1" == "tron" (echo on) else (echo off)
rem run CODEPAGE-related tests
rem     1. EBCDIC values for ISO-8859-1 A0-FF
rem          inline DC statements
rem          COPY DC statements
rem          AREAD from DAT file of values
rem          AREAD from PUNCH generated file of DC statements
rem     2. all 256 EBCDIC characters created using X2C
rem     3. no scraggly lines in SNAP output of all 256 EBCDIC chars
rem     4. non-printable chars replaced by '.' in PRN, BAL, PCH

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
call bat\asml %z_TraceMode% tests\T1P#451 bal || goto error
call bat\asml %z_TraceMode% tests\T1#451  bal || goto error
call bat\exec %z_TraceMode% tests\T1#451      || goto error

echo Verify tests\T1P#451.BAL and tests\T1#451 look okay
echo Verify console SNAP output does not contain scraggly lines

set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
