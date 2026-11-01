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
pushd %~dps0..                                       || goto nopush

rem Get version from version.txt
set /p VERSION=<version.txt                          || goto error
echo Building z390 %VERSION%
set DIST_DIR=dist

rem validate java sources
call bat\RUNLINTER.BAT %z_TraceMode%                 || goto error

rem build jar
call bat\BLDJAR.BAT   %z_TraceMode% %*               || goto error
call bat\BLDJVDOC.BAT %z_TraceMode%                  || goto error

rem remove old assembly results
rem this prevents a standard build from picking up left-over modules
del /S /Q *.obj >nul 2>&1
del /S /Q *.390 >nul 2>&1

rem generate HLASM compatible SPM's
call bat\BLDZSTRMAC.BAT %z_TraceMode% build\zstrmac  || goto error

rem build standard libraries
call bat\BLDLIB.BAT    %z_TraceMode%                 || goto error
call bat\BLDCBLLIB.BAT %z_TraceMode%                 || goto error

if /I %1. NEQ *All. goto Standard_build
   call barcode\BLD390.BAT    %z_TraceMode%          || goto error
   call bat\BLDGUAM.BAT       %z_TraceMode%          || goto error
   call vsam2\bat\BLDVSAM.BAT %z_TraceMode%          || goto error
   call cics\bat\DFHALL.BAT   %z_TraceMode%          || goto error
   call cics\bat\DFHALLV.BAT  %z_TraceMode%          || goto error
:Standard_build

rem Create/clean dist folder
echo Building distribution environment...
if not exist %DIST_DIR% (mkdir %DIST_DIR%            || goto error)
echo /** > %DIST_DIR%\.gitignore                     || goto error
if exist %DIST_DIR%\z390_%VERSION% (
   rmdir /S /Q %DIST_DIR%\z390_%VERSION%             || goto error)
if exist %DIST_DIR%\z390_%VERSION%.zip (
   del /F /Q %DIST_DIR%\z390_%VERSION%.zip           || goto error)

cd %DIST_DIR%                                        || goto error
mkdir z390_%VERSION%                                 || goto error
cd z390_%VERSION%                                    || goto error

rem add tracked files from the working tree; listings and other ignored output stay behind
setlocal EnableDelayedExpansion
for %%R in ("..\..") do set "root=%%~fR"
git -C "!root!" ls-files > "%TEMP%\z390-blddist-files.txt" || goto error
for /f "usebackq delims=" %%F in ("%TEMP%\z390-blddist-files.txt") do (
    set "rel=%%F"
    set "rel=!rel:/=\!"
    for %%D in ("!rel!") do set "folder=%%~dpD"
    if defined folder if not "!folder!"=="" if not exist "!folder!" (
        mkdir "!folder!"                             || goto error
    )
    copy /Y "!root!\!rel!" "!rel!" > nul             || goto error
)

rem add load modules produced by this build, but not copies already under dist
dir /s /b /a-d "!root!\*.390" > "%TEMP%\z390-blddist-390.txt" || goto error
for /f "usebackq delims=" %%F in ("%TEMP%\z390-blddist-390.txt") do (
    set "full=%%F"
    set "rel=!full:%root%\=!"
    echo !rel! | findstr /i /b /c:"dist\\" > nul
    if errorlevel 1 (
        for %%D in ("!rel!") do set "folder=%%~dpD"
        if defined folder if not "!folder!"=="" if not exist "!folder!" (
            mkdir "!folder!"                         || goto error
        )
        copy /Y "%%F" "!rel!" > nul                  || goto error
    )
)

rem add linklib object files and the jar; neither is tracked
if not exist linklib (mkdir linklib                  || goto error)
copy /Y "!root!\linklib\*.obj" linklib\ > nul        || goto error
copy /Y "!root!\z390.jar" .             > nul        || goto error

rem add generated structured programming macros to mac/spm folder
if not exist "mac\spm" (mkdir mac\spm                || goto error)
copy /Y ..\..\build\zstrmac\* mac\spm\ > nul         || goto error

rem add zCICS VSAM clusters produced by DFHALLV
if not exist cics\vsam (mkdir cics\vsam              || goto error)
if /I "%1"=="*All" (
    copy /Y "!root!\cics\vsam\*.v*" cics\vsam\ > nul || goto error)

rem add javadoc, including its subfolders
if not exist javadoc (mkdir javadoc                  || goto error)
dir /s /b /a-d "..\..\build\javadoc" > "%TEMP%\z390-blddist-javadoc.txt" || goto error
for /f "usebackq delims=" %%F in ("%TEMP%\z390-blddist-javadoc.txt") do (
    set "rel=%%F"
    set "rel=!rel:*\javadoc\=!"
    for %%D in ("javadoc\!rel!") do if not exist "%%~dpD" (
        mkdir "%%~dpD"                               || goto error
    )
    copy /Y "%%F" "javadoc\!rel!" > nul              || goto error
)
endlocal                                             || rem !root! and delayed expansion out of scope from here on

rem create zip package
cd ..                                                || goto error
powershell Compress-Archive -Path z390_%VERSION% -DestinationPath z390_%VERSION%.zip -Force || goto error

set z_ReturnCode=0
goto return

:nopush
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
goto exit
:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
:exit
rem ----- Lvl(%z_NestLevel%) End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
