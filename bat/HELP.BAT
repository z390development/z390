@if /I "%1" == "tron" (echo on) else (echo off)
rem provide general help text for using interface scripts

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

rem get the z390 directory
set "z_HomeDir=%~dps0..\"
for %%f in ("%z_HomeDir%") do set "z_HomeDir=%%~ff"

echo off
echo .
echo How to use the bat files in your bat directory
echo .
echo The following bat files in your bat directory are intended as a user interface:
echo ASM    - Assemble an assembler source file
echo ASML   - Assemble and link an assembler source file
echo ASMLG  - Assemble, Link and Go (execute) an assembler source file
echo ASSIST - Assemble, Link and Go (execute) an assembler source file having ASSIST opcodes
echo CBLC   - Assemble a cobol source file
echo CBLCL  - Assemble and link a cobol source file
echo CBLCLG - Assemble, Link and Go (execute) a cobol source file
echo EXEC   - Execute a previously linked program
echo HELP   - This help overview
echo LINK   - Link a previously assembled or compiled program
echo MAC    - Run the macro processor
echo Z390   - Activate the interactive z390 interface
echo .
echo All these commands (with the exception of HELP and Z390) require as their first argument
echo     the path and name of the file to be processed. The following rules apply:
echo - all output files will appear in the same directory as the input file you specified
echo - path can be absolute or relative, relative means starting from your current working directory
echo - path can be specified using either forward or backward slashes
echo - extension must be omitted; the following default extensions will be used:
echo     for assembler  files the extension is MLC
echo     for cobol      files the extension is CBL
echo     for object     files the extension is OBJ
echo     for executable files the extension is 390
echo .
echo All commands (excluding HELP, but including Z390) may optionally take additional parameters
echo These optional parameters are documented as options on the z390 web site
echo .
echo If you need additional macro and/or copybook libraries, or you need any other
echo non-default parameter for your user program, please use an options file to define these.
echo To make use of an options file, specify optional parm @path/file
echo     for options    files the extension is OPT
echo .
if /I "%1" == "tron" (echo on) else (echo off)

:return
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
