@if /I %1. == tron. (echo on
                     shift /1
             ) else (echo off
                     )
@if /I %1. == troff. shift /1
rem both the compiler javac.exe and jar builder jar.exe must be on sdk path
rem note you can use current z390.jar with compatible java runtime with sdk
REM pause press ok to rebuild z390.jar from src using current installed Java SDK 

erase ..\jar\*.java
erase ..\jar\*.class
erase ..\jar\z390.jar
xcopy ..\src\*.java   ..\jar\ /Y /Q
xcopy ..\src\z390.man ..\jar\ /Y /Q
cd    ..\jar
rem compile z390 java sources to class vm object code
echo  Compiling Classes ...
javac.exe -g:none *.java
if errorlevel 1 (pause jdk compiler error
                 goto  exit
         ) else (echo  Classes compiled okay
                 )
rem build z390.jar with sdk utility
echo  Compiling Jar ...
jar.exe cmf z390.man z390.jar *.class 
if errorlevel 1 (pause error building z390.jar
                 goto  exit
         ) else (echo  Jar compiled okay
                 )
dir *.jar
:exit
cd ..\bat
