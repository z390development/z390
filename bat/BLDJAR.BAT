@echo off
setlocal
pushd %~dp0
cd ..

rem Make jar directory if does not exist
if not exist ".\jar\" mkdir jar
rem both the compiler javac.exe and jar builder jar.exe must be on sdk path
rem note you can use current z390.jar with compatible java runtime with sdk
REM pause press ok to rebuild z390.jar from src using current installed Java SDK 
erase jar\*.java
erase jar\*.class
erase jar\z390.jar
copy  src\*.java  jar 
copy  src\z390.man jar
cd jar
rem compile z390 java sources to class vm object code
java -version
javac.exe -g:none *.java
if %ERRORLEVEL% NEQ 0 (
    echo BLDJAR   ERROR: Error compiling z390
    exit /b %ERRORLEVEL%
)
rem create z390.properties file
CALL ..\BAT\RELVER.BAT %1

rem build z390.jar with sdk utility
jar.exe cmf z390.man z390.jar *.class ..\*.properties
if %ERRORLEVEL% NEQ 0 (
    echo BLDJAR   ERROR: Error building z390.jar
    exit /b %ERRORLEVEL%
)
dir 
@REM pause press ok to copy back test\z390.jar
cd..
copy jar\z390.jar
dir z390.jar
