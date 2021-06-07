rem test and verify asml only
erase %1.ERR
erase %1.BAL
erase %1.OBJ
erase %1.PRN
erase %1.LST
erase %1.390
erase %1.TR?
call ASML %1 bal notiming stats %2 %3 %4 %5 %6 %7 %8 %9
