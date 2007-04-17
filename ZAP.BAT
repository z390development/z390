rem run superzap using %1.ZAP source input file and %1.RPT output
set  SYSIN=%1.ZAP
set  SYSPRINT=%1.RPT
call %~dps0ez390 util\superzap\SUPERZAP %2 %3 %4 %5 %6 %7 %8 %9
