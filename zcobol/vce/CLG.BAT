rem compile, link, and execute MS VCE program
set path=c:\program files\Microsoft Visual Studio 9.0\vc\bin;c:\program files\Microsoft Visual Studio 9.0\Common7\IDE;%path%
set include=c:\program files\Microsoft Visual Studio 9.0\vc\include
set lib=c:\program files\Microsoft Visual Studio 9.0\vc\lib;c:\program files\Microsoft SDKs\Windows\v6.0\lib
cl.exe /MD /Fo%1 /Fe%1 %1.cpp
if errorlevel 1 pause vce compiler and link errors
%1
