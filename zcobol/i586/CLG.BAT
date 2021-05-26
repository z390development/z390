rem assemble, link, and execute Intel assembler using HLA and MASM V8
set path=c:\hla;c:\program files\Microsoft Visual Studio 8\vc\bin;c:\program files\Microsoft Visual Studio 8\Common7\IDE;%path%
rem assemble and link using HLA, MASM, and link         
hla.exe -p: -m -E:%1 %1
if errorlevel 1 pause check hla, masm, and link errors
rem execute %1.exe
%1.exe
