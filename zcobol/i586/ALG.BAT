rem assemble, link, and execute HLA generated ASM assembler using MASM V8
set path=c:\program files\Microsoft Visual Studio 8\vc\bin;c:\program files\Microsoft Visual Studio 8\Common7\IDE;%path%
set include=c:\hla\inlcude
set lib=c:\hla\hlalib
rem MASM V8  to assemble from %1.asm to %1.obj
ml.exe /Fl%1 /Fo%1 /c /coff /Cp %1.asm
if errorlevel 1 pause check masm errors
rem link from %1.obj  to %1.exe
link.exe @%1.link -SUBSYSTEM:console -MAP:%1 -OUT:%1.exe %1 hlalib.lib
if errorlevel 1 pause check link errors
rem execute %1.exe
%1.exe
