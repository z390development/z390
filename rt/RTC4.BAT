rem test and verify asm only
erase %1.ERR
erase %1.BAL
erase %1.OBJ
erase %1.PRN
erase %1.TR?
call ASM %1 bal notiming stats %2 %3 %4 %5 %6 %7 %8 %9
