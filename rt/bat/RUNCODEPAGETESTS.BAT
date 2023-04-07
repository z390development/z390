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

pushd %~dps0..\..

set codepagedir=tests\codepage
set javadir=%codepagedir%\java
set workdir=%codepagedir%\mlcwork
set javapgm=CreateIssue451TestFiles

if not exist "%workdir%" mkdir %workdir%

:: compile program that creates the test files
javac %javadir%\%javapgm%.java
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 Error compiling %javapgm%.java
                         goto return
                         )

:: empty work directory then copy skeleton file to it
erase /q %workdir%\*.*
copy %javadir%\T1#451.MLC.SKEL %workdir%\

:: create T1#451.CPY, T1#451.DAT, T1#451.MLC and T1P#451.MLC
java -cp %javadir%\ %javapgm% %workdir%
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 Error creating test files
                         goto return
                         )

:: run the tests
call bat\asml %z_TraceMode% %workdir%\T1P#451 bal || goto error
call bat\asml %z_TraceMode% %workdir%\T1#451  bal || goto error
call bat\exec %z_TraceMode% %workdir%\T1#451      || goto error

:: get number of SNAP output lines that match search criteria
set x=0
for /f "tokens=*" %%a in (
'findstr /R /C:"^ 00..............................................................\*$" %workdir%\T1#451.LOG ^| find /c /v ""'
) do (
set x=%%a
)

:: if all 16 lines of SNAP output have same length and end with "*"
if %x% EQU 16 (
echo No scraggly lines in the SNAP output
set z_ReturnCode=0
) else (
echo Error: There are scraggly lines in the SNAP output
set z_ReturnCode=8
)

echo Verify %workdir%\T1P#451.BAL and %workdir%\T1#451.BAL look okay

goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
