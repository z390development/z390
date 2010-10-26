rem ZCJAVCLG translate CBL to JAVA, compile to CLASS via J2SE, and exec 
echo off
if %1 == tron ECHO ON
if %1 == tron shift
if %1 == TRON ECHO ON
if %1 == TRON shift
if exist %1.MLC erase %1.MLC
if exist %1.BAL erase %1.BAL
if exist %1.ERR erase %1.ERR
if exist %1.LST erase %1.LST
if exist %1.OBJ erase %1.OBJ
if exist %1.PRN erase %1.PRN
if exist %1.STA erase %1.STA
if exist %1.390 erase %1.390
if exist %1.cpp erase %1.cpp
if exist %1.java  erase %1.java    
if exist %1.class erase %1.class   
call %~dps0ZC390 %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto mzerr
set JAVAOUT=%1.JAVA
call %~dps0MZ390 %1 @zcobol\java\ZCJAVCLG %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 5 goto mzerr
"c:\program files\Java\jdk1.6.0_20\bin\javac.exe" -g:none %1.java
if errorlevel 1 goto jcerr
rem java -classpath %~p1 -Xrs %~n1 %2 %3 %4 %5 %6 %7 %8 %9
set classpath=%~p1
java -Xrs %~n1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto ezerr
goto end
:mzerr
pause see errors on mz390 generated bal file and console
goto end
:jcerr
pause see J2SE compiler errors
goto end
:ezerr
pause see errors eecuting %1 J2SE class
:end 
