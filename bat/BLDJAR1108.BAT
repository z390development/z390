cd..
pause press ok to rebuild OpenJDK 11.0.8 z390.jar from src dir 
erase jar\*.java
erase jar\*.class
erase jar\z390.jar
copy  src\*.java  jar 
copy  src\z390.man jar
cd jar
rem compile z390 java sources to class vm object code
"c:\program files\AdoptOpenJDK\jdk-11.0.8.10-hotspot\bin\javac.exe" -g:none *.java
if errorlevel 1 pause jdk-11.0.8.10 compiler error
rem build z390.jar with jdk-11.0.8.10 sdk utility
"c:\program files\AdoptOpenJDK\jdk-11.0.8.10-hotspot\bin\jar.exe" cmf z390.man z390.jar *.class 
if errorlevel 1 pause error building z390.jar
dir 
pause press ok to copy back test\z390.jar
cd..
copy  jar\z390.jar
dir z390.jar
