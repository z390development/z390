@if /I "%1" == "tron" (echo on) else (echo off)
rem Script to create z390 distribution zip file

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

rem Get version from version.txt
set /p VERSION=<version.txt
echo Building z390 %VERSION%
set DIST_DIR=dist

rem build jar
call bat\BLDJAR.BAT %z_TraceMode%        || goto error

rem generate HLASM compatible SPM's
call bat\BLDZSTRMAC.BAT %z_TraceMode% build\zstrmac || goto error

rem build standard libraries
call bat\BLDLIB.BAT %z_TraceMode%        || goto error
call bat\BLDCBLLIB.BAT %z_TraceMode%     || goto error

rem Create/clean dist folder
if not exist %DIST_DIR% mkdir %DIST_DIR%
echo /** > %DIST_DIR%\.gitignore
if exist %DIST_DIR%\z390_%VERSION% rmdir /S /Q %DIST_DIR%\z390_%VERSION%
if exist %DIST_DIR%\z390_%VERSION%.zip del /F /Q %DIST_DIR%\z390_%VERSION%.zip

cd %DIST_DIR%
mkdir z390_%VERSION%
cd z390_%VERSION%

rem add core files to the build dir
xcopy ..\..\z390.jar .   > nul
xcopy ..\..\README.txt . > nul
xcopy ..\..\LICENSE .    > nul

for %%G in (bash bat mac zcobol cics linklib) do (
    xcopy /E /I /Y ..\..\%%G %%G > nul
)

rem add test and supplemental components
for %%G in (assist demo guam qsam bsam rt soa sort structuredmacros tests vsam1 vsam2 vse zopcheck zpar) do (
    xcopy /E /I /Y ..\..\%%G %%G > nul
)

rem add structured programming macros to mac/spm folder
if not exist "mac\spm" mkdir mac\spm
xcopy /E /I /Y ..\..\build\zstrmac\* mac\spm\ > nul

rem add javadoc
mkdir javadoc
xcopy /E /I /Y ..\..\build\javadoc\* javadoc\ > nul

rem remove components not required for distribution
del /F /Q bash\bldjar bat\BLDJAR.BAT bash\blddist bash\relver bat\RELVER.BAT

rem create zip package
cd ..
powershell Compress-Archive -Path z390_%VERSION% -DestinationPath z390_%VERSION%.zip -Force || goto error

set z_ReturnCode=0
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
rem ----- Lvl(%z_NestLevel%) End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
popd
exit /b %z_ReturnCode%