pause press ok to rebuild J2SE 6.0_31 z390.jar from w:\work\eclipse\workspace\z390\src 
erase jar\*.java
erase jar\*.class
erase jar\z390.jar
copy  w:\work\eclipse\workspace\z390\src\*.java  jar 
copy w:\work\eclipse\workspace\z390\src\z390.man jar
cd jar
rem compile z390 java sources to class vm object code
"c:\program files\Java\jdk1.6.0_31\bin\javac.exe" -g:none *.java
if errorlevel 1 pause j2se 1.6.0_31 compiler error
rem build z390.jar with j2se sdk utility
"c:\program files\java\jdk1.6.0_31\bin\jar.exe" cmf z390.man z390.jar *.class 
if errorlevel 1 pause error building z390.jar
dir 
pause press ok to copy back test\z390.jar
cd..
copy  jar\z390.jar
dir z390.jar
