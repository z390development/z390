rem test and verify mz390 errors (does not stop on error)
if exist %1.BAL erase %1.BAL
if exist %1.ERR erase %1.ERR
if exist %1.STA erase %1.STA
if exist %1.TR? erase %1.TR?
call MZ390 %1 noasm bal notiming  sysmac(mac) syscpy(mac) stats %2 %3 %4 %5 %6 %7 %8 %9
