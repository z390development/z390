cd..
pause press ok to rebuild J2SE z390.jar from src using Oracle JDK 8.0.251 
erase jar\*.java
erase jar\*.class
erase jar\z390.jar
copy  src\*.java  jar 
copy  src\z390.man jar
cd jar
rem compile z390 java sources to class vm object code
"c:\program files\Java\jdk1.8.0_261\bin\javac.exe" -g:none *.java
if errorlevel 1 pause j2se 1.8.0 compiler error
rem build z390.jar with j2se sdk utility
"c:\program files\java\jdk1.8.0_261\bin\jar.exe" cmf z390.man z390.jar *.class 
if errorlevel 1 pause error building z390.jar
dir 
pause press ok to copy back test\z390.jar
cd..
copy  jar\z390.jar
dir z390.jar
