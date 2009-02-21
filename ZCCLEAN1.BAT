rem remove generated files from %1 except soures MLC,JAVA,CPP,HLA,ASMerase %1\*.BAK
echo off
erase %1\*.BAK
rem z390 generated files
if exist %1\*.BAL erase %1\*.BAL 
if exist %1\*.ERR erase %1\*.ERR 
if exist %1\*.PCH erase %1\*.PCH 
if exist %1\*.PRN erase %1\*.PRN 
if exist %1\*.OBJ erase %1\*.OBJ 
if exist %1\*.LST erase %1\*.LST 
if exist %1\*.390 erase %1\*.390 
if exist %1\*.LOG erase %1\*.LOG 
if exist %1\*.STA erase %1\*.STA 
if exist %1\*.TR? erase %1\*.TR? 
if exist %1\*.XPR erase %1\*.XPR 
if exist %1\*.TPH erase %1\*.TPH 
if exist %1\*.XPT erase %1\*.XPT 
rem java generated files
erase %1\*.CLASS
rem vce generated files
erase %1\*.LINK
erase %1\*.EXE.MANIFEST
erase %1\*.EXE 
rem i586 generated files
erase %1\*.MAP 

