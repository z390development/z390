@if /I "%1" == "tron" (echo on) else (echo off)
rem create z390 jar file from sources
rem A single parameter is allowed:
rem - NoDocs specifies to suppress generating javadoc files
rem - Debug  specifies to generate jar with debug enabled
rem --> the default is to generate javadoc and suppress debug info

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
rem both the compiler javac.exe and jar builder jar.exe must be on sdk path
rem note you can use current z390.jar with compatible java runtime with sdk

rem process options
set z_dbg=N
:opt_loop
if /I %1. EQU . goto options_ok
if /I %1. EQU Debug .  set z_dbg=Y
if /I %1. EQU Nodebug. set z_dbg=N
shift /1
goto opt_loop
:options_ok

rem prepare to compile java code and create jar file
if not exist ".\jar\" (mkdir jar                     || goto error)
cd jar                                               || goto error
if exist *.* (erase *.* /F /Q                        || goto error)

rem -- determine java version
javac -version                                       || goto error
for /f "usebackq tokens=2 delims= " %%i in (`javac -version 2^>^&1`) do (set z_ver=%%i)
for /f "tokens=1,2,3 delims=." %%a in ("%z_ver%") do (
    set z_major=%%a
    set z_minor=%%b
    set z_patch=%%c
)
if not defined z_major (
    echo %0 ERROR: cannot read javac version
    set z_ReturnCode=1
    goto return
)
if %z_major% LSS 9 (set jvopt=-source 8 -target 8) else (set jvopt=--release 8)
if /I %z_dbg% EQU Debug (set jvopt=%jvopt% -g -d .) else (set jvopt=%jvopt% -g:none -d .)

rem compile z390 java sources to class vm object code
echo javac %jvopt% ..\src\*.java
javac.exe  %jvopt% ..\src\*.java
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: Error compiling z390 java sources
                         goto return
                         )

rem create z390.properties file
call ..\bat\RELVER.BAT %z_TraceMode% %1
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: Error finding z390 version number
                         goto return
                         )

rem build z390.jar with sdk utility
jar.exe cmf ..\src\Z390.MAN z390.jar *.class ..\*.properties
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: Error creating z390.jar file
                         goto return
                         )
if not exist z390.jar   (echo %0 ERROR: Error z390.jar file failed to be created
                         set z_ReturnCode=8
                         goto return
                         )

rem back to z390 root dir
cd ..                                                || goto error

rem replace jar file in z390 root (force overwrite)
copy jar\z390.jar . /Y                               || goto error
echo %0 z390.jar has been created okay
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
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
