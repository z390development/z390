@if /I %1. == tron. (echo on) else (echo off)
rem create z390 jar file from sources

setlocal
if /I %1. == tron. (set z_TraceMode=tron
                    shift /1
            ) else (if /I %1. == troff. (set z_TraceMode=troff
                                         shift /1
                                 ) else (set z_TraceMode=)
                    )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

pushd %~dp0
cd ..

rem Make jar directory if does not exist
if not exist ".\jar\" mkdir jar
rem both the compiler javac.exe and jar builder jar.exe must be on sdk path
rem note you can use current z390.jar with compatible java runtime with sdk
erase jar\*.java
erase jar\*.class
erase jar\z390.jar
copy  src\*.java   jar
copy  src\z390.man jar
cd jar
rem compile z390 java sources to class vm object code
java -version
javac.exe -g:none *.java
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
jar.exe cmf z390.man z390.jar *.class ..\*.properties
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% NEQ 0 (echo %0 ERROR: Error creating z390.jar file
                         goto return
                         )
if not exist z390.jar   (echo %0 ERROR: Error z390.jar file failed to be created
                         set z_ReturnCode=8
                         goto return
                         )

cd..
copy jar\z390.jar
dir z390.jar
set z_ReturnCode=0

:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
