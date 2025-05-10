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

pushd %~dps0..
rem both the compiler javac.exe and jar builder jar.exe must be on sdk path
rem note you can use current z390.jar with compatible java runtime with sdk

rem process options
set z_docs=Y
set z_dbg=N
:opt_loop
if /I %1. EQU . goto options_ok
if /I %1. EQU Nodocs.  set z_docs=N
if /I %1. EQU docs.    set z_docs=Y
if /I %1. EQU Debug .  set z_dbg=Y
if /I %1. EQU Nodebug. set z_dbg=N
shift /1
goto opt_loop
:options_ok

rem compile java code and create jar file
if not exist ".\jar\" mkdir jar
cd jar
if exist *.* erase *.* /Q

rem compile z390 java sources to class vm object code
if /I %z_dbg% EQU Debug (set jvopt=-g) else (set jvopt=-g:none)
java -version
javac.exe %jvopt% -d . --release 8 ..\src\*.java
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
cd ..

rem unless suppressed, generate javadoc.
if /I %z_docs% EQU Y call bat\BLDJVDOC.BAT %z_TraceMode%
if /I %z_docs% EQU Y set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 goto return
echo .

if /I %z_docs% EQU Y (set z_msg=z390.jar and javadoc have been created okay
              ) else (set z_msg=z390.jar has been created okay
                      )
if exist jar\z390.jar (copy jar\z390.jar .\z390.jar /B /V /Y
                       echo %0 %z_msg%
                       )

rem replace jar file in z390 root (force overwrite)
copy jar\z390.jar . /Y

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
