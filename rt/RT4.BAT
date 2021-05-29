rem test and verify mz390 & az390 errors (does not stop on error)
if exist %1.BAL erase %1.BAL
if exist %1.PRN erase %1.PRN
if exist %1.OBJ erase %1.OBJ
if exist %1.ERR erase %1.ERR
if exist %1.STA erase %1.STA
if exist %1.TR? erase %1.TR?
call MZ390 %1 bal notiming sysmac(mac) syscpy(mac) stats %2 %3 %4 %5 %6 %7 %8 %9
